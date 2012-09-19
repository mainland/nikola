{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Data.Array.Nikola.Backend.CUDA.TH.Compile
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Backend.CUDA.TH.Compile
    ( evalCEx

    , compileToExpQ
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
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (ExpQ)
import Text.PrettyPrint.Mainland

import qualified Data.Array.Repa as R

import Data.Array.Nikola.Backend.C.Codegen
import Data.Array.Nikola.Backend.C.Monad hiding (gensym)
import Data.Array.Nikola.Backend.CUDA.TH.Util
import Data.Array.Nikola.Backend.Flags

import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Syntax

newtype CEx a = CEx { runCEx :: CExEnv -> IO (CExEnv, a) }

evalCEx :: CEx a -> IO ([String], [C.Definition], a)
evalCEx m = do
    (env, a) <- runCEx m emptyCExEnv
    return (cexKernels env, cexDefs env, a)

data CExEnv = CExEnv
    { cexUniq     :: !Int
    , cexKernels  :: [String]
    , cexDefs     :: [C.Definition]
    , cexVarTypes :: Map.Map Var Type
    }

emptyCExEnv :: CExEnv
emptyCExEnv = CExEnv
    { cexUniq     = 0
    , cexKernels  = []
    , cexDefs     = []
    , cexVarTypes = Map.empty
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

gensym :: String -> CEx String
gensym s = do
    u <- gets cexUniq
    modify $ \s -> s { cexUniq = u + 1 }
    return $ s ++ show u

addKernel :: ProcK -> CEx (String, [(Idx, [Exp] -> Exp)])
addKernel p = do
    fname        <- gensym "kernel"
    (idxs, defs) <- liftIO $ runC flags (compileKernelProc CUDA fname p)
    modify $ \s -> s { cexKernels = fname : cexKernels s
                     , cexDefs    = cexDefs s ++ defs
                     }
    return (fname, idxs)
  where
    flags :: Flags
    flags = defaultFlags { fOptimize = ljust 1 }

compileToExpQ :: ProcH -> CEx ([(Var, Type)], ExpQ)
compileToExpQ (ProcH vtaus p) = do
    -- liftIO $ putStrLn $ pretty 200 $ ppr (ProcH vtaus p)
    qe <- extendVarTypes vtaus $ compileProgH p
    return (vtaus, qe)

compileProgH :: ProgH -> CEx ExpQ
compileProgH (ReturnH e) = do
    qe <- compileExp e
    return [|return $qe|]

compileProgH (SeqH m1 m2) = do
    qm1 <- compileProgH m1
    qm2 <- compileProgH m2
    return [|do  $qm1
                 $qm2
            |]

compileProgH (BindH v tau m1 m2) = do
    qm1 <- compileProgH m1
    qm2 <- extendVarTypes [(v,tau)] $
           compileProgH m2
    return [|$qm1 >>= $(TH.lamE [TH.varP (TH.mkName (unVar v))] qm2)|]

compileProgH (AllocH atau e_sh) = do
    (tau, _) <- inferArrayT atau
    qe_sh    <- mapM compileExp e_sh
    return $ [|do let sh = R.shapeOfList $(TH.listE [ [|fromIntegral $qe|] | qe <- qe_sh])
                  let sz = R.size sh
                  NArray <$> $(go tau) sz <*> pure sh
              |]
  where
    go :: ScalarType -> ExpQ
    go (TupleT taus) =
        [|\sz -> $(tupM [ [|$(go tau) sz|] | tau <- taus])|]

    go _ =
        [|\sz -> liftIO $ CU.mallocForeignDevPtrArray sz|]

compileProgH (LiftH p es) = do
    (f, idxs)  <- addKernel p
    midxs      <- mapM (interpIdx es) idxs
    margs      <- mapM compileExp es
    taus       <- mapM inferExp es
    compileKernelCall f midxs (margs `zip` taus)
  where
    interpIdx :: [Exp] -> (Idx, [Exp] -> Exp) -> CEx (Idx, ExpQ)
    interpIdx es (idx, f) = do
        qe <- compileExp (f es)
        return (idx, qe)

compileProgH p = faildoc $ text "Cannot compile" <+> ppr p

compileExp :: Exp -> CEx ExpQ
compileExp (VarE v) =
    return $ TH.varE (TH.mkName (unVar v))

compileExp (ConstE c) =
    go c
  where
    go :: Const -> CEx ExpQ
    go (BoolC True)  = return [|True|]
    go (BoolC False) = return [|False|]
    go (Int8C n)     = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Int8|]
    go (Int16C n)    = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Int16|]
    go (Int32C n)    = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Int32|]
    go (Int64C n)    = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Int64|]
    go (Word8C n)    = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Word8|]
    go (Word16C n)   = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Word16|]
    go (Word32C n)   = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Word32|]
    go (Word64C n)   = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Word64|]
    go (FloatC f)    = return [|$(TH.litE (TH.rationalL (toRational   f))) :: Float|]
    go (DoubleC f)   = return [|$(TH.litE (TH.rationalL (toRational   f))) :: Double|]

compileExp UnitE =
    return [|()|]

compileExp (TupleE es) = do
    qes <- mapM compileExp es
    return $ TH.tupE qes

compileExp (DimE i _ e) = do
    (tau, _) <- inferExp e >>= inferArrayT
    qe       <- compileExp e
    return [|case $qe of { NArray _ sh -> fromIntegral (R.listOfShape sh !! i) }|]

compileExp (LetE v tau _ e1 e2) = do
    qe1 <- compileExp e1
    qe2 <- extendVarTypes [(v, tau)] $ compileExp e2
    return $ TH.letE [TH.valD (TH.varP (TH.mkName (unVar v))) (TH.normalB qe1) []] qe2

compileExp (BinopE op e1 e2) = do
    qe1 <- compileExp e1
    qe2 <- compileExp e2
    return $ go op qe1 qe2
  where
    go :: Binop -> ExpQ -> ExpQ -> ExpQ
    go Leq qe1 qe2 = [|$qe1 == $qe2|]
    go Lne qe1 qe2 = [|$qe1 /= $qe2|]
    go Lgt qe1 qe2 = [|$qe1 >  $qe2|]
    go Lge qe1 qe2 = [|$qe1 >= $qe2|]
    go Llt qe1 qe2 = [|$qe1 <  $qe2|]
    go Lle qe1 qe2 = [|$qe1 <= $qe2|]

    go Bmax qe1 qe2 = [|max $qe1 $qe2|]
    go Bmin qe1 qe2 = [|min $qe1 $qe2|]

    go Iadd qe1 qe2 = [|$qe1 + $qe2|]
    go Isub qe1 qe2 = [|$qe1 - $qe2|]
    go Imul qe1 qe2 = [|$qe1 * $qe2|]
    go Idiv qe1 qe2 = [|$qe1 `div` $qe2|]

    go Fadd qe1 qe2 = [|$qe1 + $qe2|]
    go Fsub qe1 qe2 = [|$qe1 - $qe2|]
    go Fmul qe1 qe2 = [|$qe1 * $qe2|]
    go Fdiv qe1 qe2 = [|$qe1 / $qe2|]

    go Dadd qe1 qe2 = [|$qe1 + $qe2|]
    go Dsub qe1 qe2 = [|$qe1 - $qe2|]
    go Dmul qe1 qe2 = [|$qe1 * $qe2|]
    go Ddiv qe1 qe2 = [|$qe1 / $qe2|]

    go op _ _ =
        faildoc $ text "Cannot compile:" <+> ppr op

compileExp (IfThenElseE e_test e_then e_else) = do
    qe_test <- compileExp e_test
    qe_then <- compileExp e_then
    qe_else <- compileExp e_else
    return [|if $qe_test then $qe_then else $qe_else|]

compileExp e = faildoc $ text "Cannot compile" <+> ppr e

compileKernelCall :: String -> [(Idx, ExpQ)] -> [(ExpQ, Type)] -> CEx ExpQ
compileKernelCall fname qidxs qargs = do
    let cudaIdxs = [(dim, boundsOf dim qidxs) | dim <- [CudaDimX, CudaDimY, CudaDimZ]
                                              , let bs = boundsOf dim qidxs
                                              , not (null bs)]
    (qtdims, qgdims) <- cudaGridDims cudaIdxs
    return [|do { $(allFunParams qargs) $ \fparams ->
                  CU.launchKernel $(TH.varE (TH.mkName fname)) $qtdims $qgdims 0 Nothing fparams
                }
            |]
  where
    -- Given a list of CUDA dimensiions (x, y, z) and their bounds (each
    -- dimension may be used in more than one loop, leading to more than one
    -- bound), return an action in the 'Ex' monad that yields a pair
    -- consisting of the thread block dimensions and the grid dimensions,
    cudaGridDims :: [(CudaDim, [ExpQ])] -> CEx (ExpQ, ExpQ)
    cudaGridDims []              = return ([|(1, 1, 1)|],   [|(1, 1, 1)|])
    cudaGridDims [(CudaDimX, _)] = return ([|(480, 1, 1)|], [|(128, 1, 1)|])
    cudaGridDims _               = error "cudaGridDims: failed to compute grid dimensions"

    -- Given a CUDA dimension (x, y, or z) and a list of indices and their
    -- bounds, return the list of bounds for the given CUDA dimension.
    boundsOf :: CudaDim -> [(Idx, ExpQ)] -> [ExpQ]
    boundsOf dim idxs = [val | (CudaThreadIdx dim', val) <- idxs, dim' == dim]

    allFunParams :: [(ExpQ, Type)] -> ExpQ
    allFunParams []           = [|\kont -> kont []|]
    allFunParams ((qe,_):qes) = [|\kont -> toFunParams $qe     $ \fparams1 ->
                                           $(allFunParams qes) $ \fparams2 ->
                                           kont (fparams1 ++ fparams2)|]

instance Pretty CU.FunParam where
    ppr (CU.IArg i)  = text "IArg" <+> ppr i
    ppr (CU.FArg f)  = text "FArg" <+> ppr f
    ppr (CU.VArg {}) = text "VArg"
    ppr _            = text "TArg"

instance Show CU.FunParam where
    show = show . ppr
