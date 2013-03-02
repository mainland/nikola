{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Data.Bits
import Data.Int
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Word
import Foreign (Ptr, alloca, poke)
import qualified Foreign.CUDA.Driver as CU
import qualified Foreign.CUDA.ForeignPtr as CU
import qualified Language.C.Syntax as C
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH (ExpQ, PatQ, TypeQ)
import Text.PrettyPrint.Mainland

import qualified Data.Array.Repa as R

import Data.Array.Nikola.Backend.C.Codegen
import Data.Array.Nikola.Backend.C.Monad hiding (gensym)
import Data.Array.Nikola.Backend.CUDA.TH.Util
import Data.Array.Nikola.Backend.Flags

import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Syntax

newtype CEx a = CEx { runCEx :: CExEnv -> IO (CExEnv, a) }

evalCEx :: CEx a -> IO ([CudaKernel], [C.Definition], a)
evalCEx m = do
    (env, a) <- runCEx m defaultCExEnv
    return (cexKernels env, cenvToCUnit (cexCEnv env), a)

data CExEnv = CExEnv
    { cexUniq     :: !Int
    , cexKernels  :: [CudaKernel]
    , cexCEnv     :: CEnv
    , cexContext  :: Context
    , cexVarTypes :: Map.Map Var Type
    }

defaultCExEnv :: CExEnv
defaultCExEnv = CExEnv
    { cexUniq     = 0
    , cexKernels  = []
    , cexCEnv     = defaultCEnv flags
    , cexContext  = Host
    , cexVarTypes = Map.empty
    }
  where
    flags :: Flags
    flags = defaultFlags { fOptimize = ljust 1 }

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
    getContext = gets cexContext

    setContext ctx = modify $ \s -> s { cexContext = ctx }

    lookupVarType v = do
        maybe_tau <- gets $ \s -> Map.lookup v (cexVarTypes s)
        case maybe_tau of
          Just tau -> return tau
          Nothing ->  faildoc $ text "Variable" <+> ppr v <+>
                                text "not in scope."

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

addKernel :: Exp -> CEx CudaKernel
addKernel f = do
    kname         <- gensym "kernel"
    cenv          <- gets cexCEnv
    (kern, cenv') <- liftIO $ runC (compileKernelFun CUDA kname f) cenv
    modify $ \s -> s { cexKernels = kern : cexKernels s
                     , cexCEnv    = cenv'
                     }
    return kern

compileToExpQ :: Exp -> CEx ([(Var, Type)], ExpQ, Bool)
compileToExpQ e = do
    tau_ret <- extendVarTypes vtaus $
               inferExp body
    qe      <- extendVarTypes vtaus $
               compileExp body
    return (vtaus, qe, isMT tau_ret)
  where
    (vtaus, body) = splitLamE e

compileConst :: Const -> CEx ExpQ
compileConst (BoolC True)  = return [|True|]
compileConst (BoolC False) = return [|False|]
compileConst (Int8C n)     = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Int8|]
compileConst (Int16C n)    = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Int16|]
compileConst (Int32C n)    = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Int32|]
compileConst (Int64C n)    = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Int64|]
compileConst (Word8C n)    = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Word8|]
compileConst (Word16C n)   = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Word16|]
compileConst (Word32C n)   = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Word32|]
compileConst (Word64C n)   = return [|$(TH.litE (TH.integerL  (fromIntegral n))) :: Word64|]
compileConst (FloatC f)    = return [|$(TH.litE (TH.rationalL (toRational   f))) :: Float|]
compileConst (DoubleC f)   = return [|$(TH.litE (TH.rationalL (toRational   f))) :: Double|]

compileExp :: Exp -> CEx ExpQ
compileExp (VarE v) =
    return $ TH.varE (TH.mkName (unVar v))

compileExp (ConstE c) =
    compileConst c

compileExp UnitE =
    return [|()|]

compileExp (TupleE es) = do
    qes <- mapM compileExp es
    return $ TH.tupE qes

compileExp (LetE v tau _ e1 e2) = do
    qe1 <- compileExp e1
    qe2 <- extendVarTypes [(v, tau)] $
           compileExp e2
    return $ TH.letE [TH.valD (TH.varP (TH.mkName (unVar v))) (TH.normalB qe1) []] qe2

compileExp e0@(CallE f es) = do
    tau_ret       <- inferExp e0
    when (tau_ret /= MT unitT) $
        faildoc $
            text "Kernel returns a non-unit value of type" <+>
            ppr tau_ret <> text "." <+>
            text "I can't handle that yet."
    kern           <- addKernel f
    (gdims, tdims) <- liftIO $ calcKernelDims kern es
    qargs          <- mapM compileExp es
    taus           <- mapM inferExp es
    return [|do { $(initWorkBlockCounters (cukernWorkBlocks kern))
                ; $(allFunParams (qargs `zip` taus)) $ \fparams ->
                  CU.launchKernel $(TH.varE (TH.mkName (cukernName kern)))
                                  $(TH.lift gdims)
                                  $(TH.lift tdims)
                                  0 Nothing fparams
                }
            |]
  where
    initWorkBlockCounters :: [CudaWorkBlock] -> ExpQ
    initWorkBlockCounters [] =
        [|return ()|]

    initWorkBlockCounters (wb:wbs) =
        [|alloca (\ptr -> poke ptr 0 >> CU.pokeArray 1 (ptr :: Ptr Int32) $(TH.varE blockCounterName))
          >> $(initWorkBlockCounters wbs)|]
      where
        blockCounterName :: TH.Name
        blockCounterName = TH.mkName (cuworkBlockCounter wb)

    allFunParams :: [(ExpQ, Type)] -> ExpQ
    allFunParams []           = [|\kont -> kont []|]
    allFunParams ((qe,_):qes) = [|\kont -> toFunParams $qe     $ \fparams1 ->
                                           $(allFunParams qes) $ \fparams2 ->
                                           kont (fparams1 ++ fparams2)|]

compileExp (BinopE op e1 e2) = do
    qe1 <- compileExp e1
    qe2 <- compileExp e2
    return $ go op qe1 qe2
  where
    go :: Binop -> ExpQ -> ExpQ -> ExpQ
    go EqO qe1 qe2 = [|$qe1 == $qe2|]
    go NeO qe1 qe2 = [|$qe1 /= $qe2|]
    go GtO qe1 qe2 = [|$qe1 >  $qe2|]
    go GeO qe1 qe2 = [|$qe1 >= $qe2|]
    go LtO qe1 qe2 = [|$qe1 <  $qe2|]
    go LeO qe1 qe2 = [|$qe1 <= $qe2|]

    go MaxO qe1 qe2 = [|max $qe1 $qe2|]
    go MinO qe1 qe2 = [|min $qe1 $qe2|]

    go AndL qe1 qe2 = [|$qe1 && $qe2|]
    go OrL  qe1 qe2 = [|$qe1 || $qe2|]

    go AddN qe1 qe2 = [|$qe1 + $qe2|]
    go SubN qe1 qe2 = [|$qe1 - $qe2|]
    go MulN qe1 qe2 = [|$qe1 * $qe2|]

    go AndB qe1 qe2 = [|$qe1 .&. $qe2|]
    go OrB  qe1 qe2 = [|$qe1 .|. $qe2|]

    go QuotI qe1 qe2 = [|$qe1 `quot` $qe2|]
    go RemI  qe1 qe2 = [|$qe1 `rem` $qe2|]

    go DivF     qe1 qe2 = [|$qe1 / $qe2|]
    go ModF     qe1 qe2 = [|let { x = $qe1 ; y = $qe2 }
                            in x - (fromIntegral (floor (x/y)))*y|]
    go PowF     qe1 qe2 = [|$qe1 ** $qe2|]
    go LogBaseF qe1 qe2 = [|logBase $qe1 $qe2|]

compileExp (IfThenElseE e_test e_then e_else) = do
    qe_test <- compileExp e_test
    qe_then <- compileExp e_then
    qe_else <- compileExp e_else
    return [|if $qe_test then $qe_then else $qe_else|]

compileExp (ReturnE e) = do
    qe <- compileExp e
    return [|return $qe|]

compileExp (SeqE m1 m2) = do
    qm1 <- compileExp m1
    qm2 <- compileExp m2
    return [|$qm1 >> $qm2|]

compileExp (ParE m1 m2) = do
    qm1 <- compileExp m1
    qm2 <- compileExp m2
    return [|$qm1 >> $qm2|]

compileExp (BindE v tau m1 m2) = do
    qm1 <- compileExp m1
    qm2 <- extendVarTypes [(v,tau)] $
           compileExp m2
    return [|do { x <- $qm1
                ; return $(sigE [|x|] tau)
                }
             >>= $(TH.lamE [TH.varP (TH.mkName (unVar v))] qm2)|]

compileExp (AllocE atau e_sh) = do
    (tau, _) <- checkArrayT atau
    qe_sh    <- mapM compileExp e_sh
    return [|do { let sh = R.shapeOfList $(TH.listE [ [|fromIntegral ($qe :: Int32)|] | qe <- qe_sh])
                ; let sz = R.size sh
                ; NArray <$> $(go tau) sz <*> pure sh
                }
            |]
  where
    go :: ScalarType -> ExpQ
    go (TupleT taus) =
        [|\sz -> $(tupM [ [|$(go tau) sz|] | tau <- taus])|]

    go tau =
        [|\sz -> liftIO $ do { arr <- CU.mallocForeignDevPtrArray sz
                             ; return $(sigE [|arr|] (PtrT tau))
                             }
         |]

compileExp (DimE i _ e) = do
    (tau, _) <- inferExp e >>= checkArrayT
    qe       <- compileExp e
    return [|case $qe of { NArray _ sh -> fromIntegral (R.listOfShape sh !! i) }|]

compileExp e =
    faildoc $ nest 4 $
    text "Cannot compile the following Nikola term to a Haskell term:" </> ppr e

-- | Conversion to Haskell type
class IsHType a where
    toHType :: a -> TypeQ

instance IsHType ScalarType where
    toHType UnitT         = [t|()|]
    toHType BoolT         = [t|Bool|]
    toHType Int8T         = [t|Int8|]
    toHType Int16T        = [t|Int16|]
    toHType Int32T        = [t|Int32|]
    toHType Int64T        = [t|Int64|]
    toHType Word8T        = [t|Word8|]
    toHType Word16T       = [t|Word16|]
    toHType Word32T       = [t|Word32|]
    toHType Word64T       = [t|Word64|]
    toHType FloatT        = [t|Float|]
    toHType DoubleT       = [t|Double|]
    toHType (TupleT taus) = tupsT (map toHType taus)

instance IsHType PtrType where
    toHType (PtrT (TupleT taus)) = tupsT [toHType (PtrT tau) | tau <- taus]
    toHType (PtrT tau)           = TH.appT (TH.conT ''CU.ForeignDevicePtr) (toHType tau)

instance IsHType Type where
    toHType (ScalarT tau)  = toHType tau
    toHType (ArrayT tau n) = appsT [[t|NikolaArray|], toHType (PtrT tau), dimType n]
      where
        dimType :: Int -> TypeQ
        dimType 0 = [t|R.Z|]
        dimType n = appsT [[t|(R.:.)|], dimType (n-1), [t|Int|]]
    toHType tau = faildoc $ text "Cannot convert" <+> ppr tau <+> text "to a Haskell type"

appsT :: [TypeQ] -> TypeQ
appsT = foldl1 TH.appT

tupsT :: [TypeQ] -> TypeQ
tupsT taus = foldl TH.appT (TH.tupleT (length taus)) taus

sigE :: IsHType tau => ExpQ -> tau -> ExpQ
sigE qe tau = TH.sigE qe (toHType tau)

sigP :: IsHType tau => PatQ -> tau -> PatQ
sigP qp tau = TH.sigP qp (toHType tau)
