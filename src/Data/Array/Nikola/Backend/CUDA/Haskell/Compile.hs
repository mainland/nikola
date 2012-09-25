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
    (tau, _) <- checkArrayT atau
    m_sh     <- mapM compileExp e_sh
    return $ do  sh   <- sequence m_sh >>= mapM fromIx
                 ptrs <- go (product sh) tau
                 return $ ArrayV ptrs sh
  where
    go :: Int -> ScalarType -> Ex PtrVal
    go sz (TupleT taus) =
        TupPtrV <$> mapM (go sz) taus

    go sz tau = do
        ptr :: CU.ForeignDevicePtr Word8 <-
               liftIO $ CU.mallocForeignDevPtrArray (sz*sizeOfT tau)
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
    tau <- inferExp e1
    m1  <- compileExp e1
    m2  <- compileExp e2
    return $ go tau op m1 m2
  where
    go :: Type -> Binop -> Ex Val -> Ex Val -> Ex Val
    go _ EqO m1 m2 = liftOrd (==) m1 m2
    go _ NeO m1 m2 = liftOrd (/=) m1 m2
    go _ GtO m1 m2 = liftOrd (>)  m1 m2
    go _ GeO m1 m2 = liftOrd (>=) m1 m2
    go _ LtO m1 m2 = liftOrd (<)  m1 m2
    go _ LeO m1 m2 = liftOrd (<=) m1 m2

    go _ MaxO m1 m2 = liftMaxMin max m1 m2
    go _ MinO m1 m2 = liftMaxMin min m1 m2

    go _   AddN m1 m2 = liftNum (+) m1 m2
    go _   SubN m1 m2 = liftNum (-) m1 m2
    go _   MulN m1 m2 = liftNum (*) m1 m2
    go tau DivN m1 m2
        | isIntT tau = liftIntegral div m1 m2
        | otherwise  = liftFloating (/) m1 m2

    go _ op _ _ =
        faildoc $ text "Cannot compile:" <+> ppr op

compileExp (IfThenElseE e_test e_then e_else) = do
    m_test <- compileExp e_test
    m_then <- compileExp e_then
    m_else <- compileExp e_else
    return $ do  BoolV v_test <- m_test
                 if v_test then m_then else m_else

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
                 liftIO $ toFunParams args $ \fparams ->
                          CU.launchKernel f tdims gdims 0 Nothing fparams
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

class ToFunParams a where
    toFunParams :: a -> ([CU.FunParam] -> IO b) -> IO b

instance ToFunParams a => ToFunParams [a] where
    toFunParams [] kont =
        kont []

    toFunParams (x:xs) kont =
        toFunParams x  $ \fparams_x  ->
        toFunParams xs $ \fparams_xs ->
        kont $ fparams_x ++ fparams_xs

instance ToFunParams PtrVal where
    toFunParams (PtrV fdptr) kont =
        CU.withForeignDevPtr fdptr $ \dptr ->
        kont [CU.VArg dptr]

    toFunParams (TupPtrV ptrs) kont =
        toFunParams ptrs kont

instance ToFunParams Val where
    toFunParams UnitV            _    = error "Cannot pass unit type to CUDA function"
    toFunParams (BoolV False)    kont = kont [CU.VArg (0 :: Word8)]
    toFunParams (BoolV True)     kont = kont [CU.VArg (1 :: Word8)]
    toFunParams (Int8V  n)       kont = kont [CU.VArg n]
    toFunParams (Int16V n)       kont = kont [CU.VArg n]
    toFunParams (Int32V n)       kont = kont [CU.VArg n]
    toFunParams (Int64V n)       kont = kont [CU.VArg n]
    toFunParams (Word8V  n)      kont = kont [CU.VArg n]
    toFunParams (Word16V n)      kont = kont [CU.VArg n]
    toFunParams (Word32V n)      kont = kont [CU.VArg n]
    toFunParams (Word64V n)      kont = kont [CU.VArg n]
    toFunParams (FloatV f)       kont = kont [CU.VArg f]
    toFunParams (DoubleV f)      kont = kont [CU.VArg f]
    toFunParams (TupleV vals)    kont = toFunParams vals kont
    toFunParams (ArrayV ptrs sh) kont = toFunParams ptrs $ \fparams_ptrs ->
                                        let fparams_sh = [CU.VArg (fromIntegral i :: Int32) | i <- sh]
                                        in
                                          kont $ fparams_ptrs ++ fparams_sh
    toFunParams (FunV {})        _    = error "Cannot pass function value to kernel"
