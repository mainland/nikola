{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Array.Nikola.Backend.CUDA.Haskell.Compile
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Backend.CUDA.Haskell.Compile
    ( CEx
    , runCEx
    , evalCEx

    , compileToEx
    ) where

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad.State
import Data.Int
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Word
import qualified Foreign.CUDA.Driver as CU
import qualified Foreign.CUDA.ForeignPtr as CU
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Backend.C.Codegen
import Data.Array.Nikola.Backend.C.Monad hiding (gensym)
import Data.Array.Nikola.Backend.CUDA (sizeOfT)
import Data.Array.Nikola.Backend.CUDA.Haskell.Ex
import Data.Array.Nikola.Backend.Flags

import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Syntax

newtype CEx a = CEx { runCEx :: CExEnv -> IO (CExEnv, a) }

evalCEx :: CEx a -> IO ([C.Definition], a)
evalCEx m = do
    (env, a) <- runCEx m emptyCExEnv
    return (cexDefs env, a)

data CExEnv = CExEnv
    {  cexUniq     :: !Int
    ,  cexDefs     :: [C.Definition]
    ,  cexVarTypes :: Map.Map Var Type
    ,  cexVarIdxs  :: Map.Map Var Int
    }

emptyCExEnv :: CExEnv
emptyCExEnv = CExEnv
    { cexUniq     = 0
    , cexDefs     = []
    , cexVarTypes = Map.empty
    , cexVarIdxs  = Map.empty
    }

instance Monad CEx where
    return a = CEx $ \s -> return (s, a)

    m >>= f  = CEx $ \s -> do  (s', x) <- runCEx m s
                               runCEx (f x) s'

    m1 >> m2 = CEx $ \s -> do  (s', _) <- runCEx m1 s
                               runCEx m2 s'

    fail err = CEx $ \_ -> fail err

instance Functor CEx where
    fmap f x = x >>= return . f

instance Applicative CEx where
    pure   = return
    (<*>)  = ap

instance MonadState CExEnv CEx where
    get   = CEx $ \s -> return (s, s)
    put s = CEx $ \_ -> return (s, ())

instance MonadIO CEx where
    liftIO m = CEx $ \s -> do x <- m
                              return (s, x)

instance MonadCheck CEx where
    lookupVarType v = do
        maybe_tau <- gets $ \s -> Map.lookup v (cexVarTypes s)
        case maybe_tau of
          Just tau -> return tau
          Nothing ->  faildoc $ text "Variable" <+> ppr v <+>
                                text "not in scope during reification."

    extendVarTypes vtaus act = do
        old_vars <- gets cexVarTypes
        modify $ \s -> s { cexVarTypes = foldl' insert (cexVarTypes s) vtaus }
        x  <- act
        modify $ \s -> s { cexVarTypes = old_vars }
        return x
      where
        insert m (k, v) = Map.insert k v m

extendVars :: [(Var, Type)] -> CEx a -> CEx a
extendVars vtaus act = do
    old_vars <- gets cexVarTypes
    old_idxs <- gets cexVarIdxs
    let n = length vtaus
    modify $ \s -> s { cexVarTypes = foldl' insert (cexVarTypes s) vtaus
                     , cexVarIdxs  = foldl' insert
                                       (Map.map (+n) (cexVarIdxs s))
                                       (map fst vtaus `zip` [0..n-1])
                     }
    x  <- act
    modify $ \s -> s { cexVarTypes = old_vars
                     , cexVarIdxs  = old_idxs
                     }
    return x
  where
    insert m (k, v) = Map.insert k v m

lookupVar :: Var -> CEx Int
lookupVar v = do
    maybe_i <- gets $ Map.lookup v . cexVarIdxs
    case maybe_i of
      Nothing -> faildoc $ text "Cannot find argument index for" <+> ppr v
      Just i  -> i `seq` return i

gensym :: String -> CEx String
gensym s = do
    u <- gets cexUniq
    modify $ \s -> s { cexUniq = u + 1 }
    return $ s ++ show u

addKernel :: ProcK -> CEx (String, [(Idx, [Exp] -> Exp)])
addKernel p = do
    fname        <- gensym "kernel"
    (idxs, defs) <- liftIO $ runC flags (compileKernelProc CUDA fname p)
    modify $ \s -> s { cexDefs = cexDefs s ++ defs }
    return (fname, idxs)
  where
    flags :: Flags
    flags = defaultFlags { fOptimize = ljust 1 }

fromIx :: Val -> Ex Int
fromIx (Int32V n) = return (fromIntegral n)
fromIx _          = faildoc $ text "internal error: fromIx"

toIx :: Int -> Ex Val
toIx n = return $ Int32V (fromIntegral n)

-- 'compileToEx' takes a host procedure and compiles it to a monadic action of
-- type 'Ex Val' in the 'Ex' monad'. This monadic action coordinates execution
-- the GPU kernels. It isn't particularly efficient---if you want efficiency,
-- use the Template Haskell back-end in
-- "Data.Array.Nikola.Backend.CUDA.TH.Compile".
compileToEx :: ProcH -> CEx (Ex Val)
compileToEx (ProcH vtaus p) =
    extendVars vtaus $
    compileProgH p

compileProgH :: ProgH -> CEx (Ex Val)
compileProgH (ReturnH e) =
    compileExp e

compileProgH (SeqH m1 m2) = do
    f <- compileProgH m1
    g <- compileProgH m2
    return $ f >> g

compileProgH (BindH v tau m1 m2) = do
    f <- compileProgH m1
    g <- extendVars [(v,tau)] $
         compileProgH m2
    return $ do  f >>= pushVal
                 g

compileProgH (AllocH atau e_sh) = do
    (tau, _) <- inferArrayT atau
    m_sh     <- mapM compileExp e_sh
    return $ do  sh   <- sequence m_sh >>= mapM fromIx
                 ptrs <- go (product sh) tau
                 return $ ArrayV ptrs sh
  where
    go :: Int -> ScalarType -> Ex PtrVal
    go sz (TupleT taus) =
        TupPtrV <$> mapM (go sz) taus

    go sz tau = do
        ptr :: CU.ForeignDevicePtr Word8 <- liftIO $ CU.mallocForeignDevPtrArray (sz*sizeOfT tau)
        return $ PtrV (CU.castForeignDevPtr ptr)

compileProgH (LiftH p es) = do
    (f, idxs)  <- addKernel p
    midxs      <- mapM (interpIdx es) idxs
    margs      <- mapM compileExp es
    compileKernelCall f midxs margs
  where
    interpIdx :: [Exp] -> (Idx, [Exp] -> Exp) -> CEx (Idx, Ex Val)
    interpIdx es (idx, f) = do
        val <- compileExp (f es)
        return (idx, val)

compileProgH m = faildoc $ text "Cannot compile:" <+> ppr m

compileExp :: Exp -> CEx (Ex Val)
compileExp (VarE v) = do
    i <- lookupVar v
    return $ gets (\s -> exVals s !! i)

compileExp (ConstE c) =
    go c
  where
    go :: Const -> CEx (Ex Val)
    go (BoolC b)   = return $ return $ BoolV   b
    go (Int8C n)   = return $ return $ Int8V   n
    go (Int16C n)  = return $ return $ Int16V  n
    go (Int32C n)  = return $ return $ Int32V  n
    go (Int64C n)  = return $ return $ Int64V  n
    go (Word8C n)  = return $ return $ Word8V   n
    go (Word16C n) = return $ return $ Word16V  n
    go (Word32C n) = return $ return $ Word32V  n
    go (Word64C n) = return $ return $ Word64V  n
    go (FloatC f)  = return $ return $ FloatV  f
    go (DoubleC f) = return $ return $ DoubleV f

compileExp UnitE =
    return $ return UnitV

compileExp (DimE i _ e) = do
    me <- compileExp e
    return $ do  ArrayV _ sh <- me
                 toIx (sh !! i)

compileExp (LetE v tau _ e1 e2) = do
    f <- compileExp e1
    g <- extendVars [(v,tau)] $
         compileExp e2
    return $ do  f >>= pushVal
                 x <- g
                 popVal
                 return x

compileExp (LamE vtaus e) = do
    f <- extendVars vtaus $ compileExp e
    return $ return (FunV f)

compileExp (AppE f es) = do
    mf  <- compileExp f
    mes <- mapM compileExp es
    return $ do  FunV f <- mf
                 vals   <- sequence mes
                 mapM_ pushVal (reverse vals)
                 x <- f
                 popVals (length vals)
                 return x

compileExp (BinopE op e1 e2) = do
    m1 <- compileExp e1
    m2 <- compileExp e2
    return $ go op m1 m2
  where
    go :: Binop -> Ex Val -> Ex Val -> Ex Val
    go Bmax m1 m2 = do
        val1 <- m1
        val2 <- m2
        case (val1, val2) of
          (Int8V n1,   Int8V n2)   -> return $ Int8V   (max n1 n2)
          (Int16V n1,  Int16V n2)  -> return $ Int16V  (max n1 n2)
          (Int32V n1,  Int32V n2)  -> return $ Int32V  (max n1 n2)
          (Int64V n1,  Int64V n2)  -> return $ Int64V  (max n1 n2)
          (Word8V n1,  Word8V n2)  -> return $ Word8V   (max n1 n2)
          (Word16V n1, Word16V n2) -> return $ Word16V  (max n1 n2)
          (Word32V n1, Word32V n2) -> return $ Word32V  (max n1 n2)
          (Word64V n1, Word64V n2) -> return $ Word64V  (max n1 n2)
          (FloatV n1,  FloatV n2)  -> return $ FloatV  (max n1 n2)
          (DoubleV n1, DoubleV n2) -> return $ DoubleV (max n1 n2)
          _ -> faildoc $ text "internal error: max" <+> ppr val1 <+> ppr val2

    go Iadd m1 m2 = int32Op (+) m1 m2
    go Isub m1 m2 = int32Op (-) m1 m2
    go Imul m1 m2 = int32Op (*) m1 m2
    go Idiv m1 m2 = int32Op div m1 m2

    go Fadd m1 m2 = floatOp (+) m1 m2
    go Fsub m1 m2 = floatOp (-) m1 m2
    go Fmul m1 m2 = floatOp (*) m1 m2
    go Fdiv m1 m2 = floatOp (/) m1 m2

    go Dadd m1 m2 = doubleOp (+) m1 m2
    go Dsub m1 m2 = doubleOp (-) m1 m2
    go Dmul m1 m2 = doubleOp (*) m1 m2
    go Ddiv m1 m2 = doubleOp (/) m1 m2

    go op _ _ =
        faildoc $ text "Cannot compile:" <+> ppr op

    int32Op :: (Int32 -> Int32 -> Int32) -> Ex Val -> Ex Val -> Ex Val
    int32Op op m1 m2 = do
        Int32V n1 <- m1
        Int32V n2 <- m2
        return $ Int32V (op n1 n2)

    floatOp :: (Float -> Float -> Float) -> Ex Val -> Ex Val -> Ex Val
    floatOp op m1 m2 = do
        FloatV n1 <- m1
        FloatV n2 <- m2
        return $ FloatV (op n1 n2)

    doubleOp :: (Double -> Double -> Double) -> Ex Val -> Ex Val -> Ex Val
    doubleOp op m1 m2 = do
        DoubleV n1 <- m1
        DoubleV n2 <- m2
        return $ DoubleV (op n1 n2)

compileExp e = faildoc $ text "Cannot compile:" <+> ppr e

instance Pretty CU.FunParam where
    ppr (CU.IArg i)  = text "IArg" <+> ppr i
    ppr (CU.FArg f)  = text "FArg" <+> ppr f
    ppr (CU.VArg {}) = text "VArg"
    ppr _            = text "TArg"

compileKernelCall :: String -> [(Idx, Ex Val)] -> [Ex Val] -> CEx (Ex Val)
compileKernelCall fname midxs margs = do
    let cudaIdxs = [(dim, boundsOf dim midxs) | dim <- [CudaDimX, CudaDimY, CudaDimZ]
                                              , let bs = boundsOf dim midxs
                                              , not (null bs)]
    mdims <- cudaGridDims cudaIdxs
    return $ do  mod            <- getCUDAModule
                 f              <- liftIO $ CU.getFun mod fname
                 (tdims, gdims) <- mdims
                 args           <- sequence margs
                 -- liftIO $ putStrLn $ pretty 200 $ ppr $ concatMap valToFunParams args
                 liftIO $ CU.launchKernel f tdims gdims 0 Nothing (concatMap valToFunParams args)
                 return UnitV
  where
    -- Given a list of CUDA dimensiions (x, y, z) and their bounds (each
    -- dimension may be used in more than one loop, leading to more than one
    -- bound), return an action in the 'Ex' monad that yields a pair
    -- consisting of the thread block dimensions and the grid dimensions,
    cudaGridDims :: [(CudaDim, [Ex Val])] -> CEx (Ex ((Int, Int, Int), (Int, Int, Int)))
    cudaGridDims []              = return $ return ((1, 1, 1), (1, 1, 1))
    cudaGridDims [(CudaDimX, _)] = return $ return ((480, 1, 1), (128, 1, 1))
    cudaGridDims _               = error "cudaGridDims: failed to compute grid dimensions"

    -- Given a CUDA dimension (x, y, or z) and a list of indices and their
    -- bounds, return the list of bounds for the given CUDA dimension.
    boundsOf :: CudaDim -> [(Idx, Ex Val)] -> [Ex Val]
    boundsOf dim idxs = [val | (CudaThreadIdx dim', val) <- idxs, dim' == dim]

    -- Convert a 'Val' to a 'CU.FunParam'.
    ptrValToFunParams :: PtrVal -> [CU.FunParam]
    ptrValToFunParams (PtrV fdptr)   = [CU.VArg (CU.unsafeForeignDevPtrToDevPtr fdptr)]
    ptrValToFunParams (TupPtrV ptrs) = concatMap ptrValToFunParams ptrs

    valToFunParams :: Val -> [CU.FunParam]
    valToFunParams UnitV            = error "Cannot pass unit type to CUDA function"
    valToFunParams (BoolV False)    = [CU.VArg (0 :: Int32)]
    valToFunParams (BoolV True)     = [CU.VArg (1 :: Int32)]
    valToFunParams (Int8V  n)       = [CU.VArg n]
    valToFunParams (Int16V n)       = [CU.VArg n]
    valToFunParams (Int32V n)       = [CU.VArg n]
    valToFunParams (Int64V n)       = [CU.VArg n]
    valToFunParams (Word8V  n)      = [CU.VArg n]
    valToFunParams (Word16V n)      = [CU.VArg n]
    valToFunParams (Word32V n)      = [CU.VArg n]
    valToFunParams (Word64V n)      = [CU.VArg n]
    valToFunParams (FloatV f)       = [CU.VArg f]
    valToFunParams (DoubleV f)      = [CU.VArg f]
    valToFunParams (TupleV vals)    = concatMap valToFunParams vals
    valToFunParams (ArrayV ptrs sh) = ptrValToFunParams ptrs ++
                                      [CU.VArg (fromIntegral i :: Int32) | i <- sh]
    valToFunParams (FunV {})        = error "Cannot pass function value to kernel"
