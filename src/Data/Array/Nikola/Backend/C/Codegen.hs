{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Backend.C.Codegen
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Backend.C.Codegen (
    compileProgram,
    compileKernelFun
  ) where

import Control.Applicative ((<$>),
                            (<*>))
import Control.Monad (replicateM,
                      when,
                      zipWithM_)
import Data.Functor.Identity
import Data.List (findIndex)
import Data.Monoid (Last(..), Sum(..))
import Language.C.Quote.C
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland

#if !MIN_VERSION_template_haskell(2,7,0)
import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax
#endif /* !MIN_VERSION_template_haskell(2,7,0) */

import Data.Array.Nikola.Backend.C.Monad
import Data.Array.Nikola.Backend.C.Quoters
import Data.Array.Nikola.Backend.Flags

import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Syntax
-- import Data.Array.Nikola.Pretty

-- Compile a program to a C function
compileProgram :: Flags -> Exp -> IO [C.Definition]
compileProgram flags p = do
    snd <$> runC flags go
  where
    (vtaus, body) = splitLamE p

    go :: C CExp
    go = do
        let dialect = fromLJust fDialect flags
        addIncludes dialect
        flags    <- getFlags
        fname    <- case fFunction flags of
                      Last Nothing      -> gensym "host"
                      Last (Just fname) -> return fname
        tau_host <- inferExp p
        tau_ret  <- snd <$> checkFunT tau_host
        compileFun dialect Host Host fname vtaus tau_ret $ do
        declareHeap dialect (numAllocs p)
        declareResult dialect
        addFinalStm (returnResult dialect)
        addFinalStm [cstm|done: $stm:gc|]
        ce <- compileExp body
        mark ce
        return ce

    addIncludes :: Dialect -> C ()
    addIncludes CUDA = do
        addInclude "\"cuda.h\""
        addInclude "\"cuda_runtime_api.h\""

    addIncludes OpenMP = do
        addInclude "<stdlib.h>"
        addInclude "<inttypes.h>"
        addInclude "<math.h>"
        addInclude "<omp.h>"

    addIncludes _ =
        return ()

-- Compile a constant to a C expression
compileConst :: Const -> C CExp
compileConst (BoolC True)  = return $ ScalarCE [cexp|0|]
compileConst (BoolC False) = return $ ScalarCE [cexp|1|]
compileConst (Int8C n)     = return $ ScalarCE [cexp|$int:(toInteger n)|]
compileConst (Int16C n)    = return $ ScalarCE [cexp|$int:(toInteger n)|]
compileConst (Int32C n)    = return $ ScalarCE [cexp|$int:(toInteger n)|]
compileConst (Int64C n)    = return $ ScalarCE [cexp|$lint:(toInteger n)|]
compileConst (Word8C n)    = return $ ScalarCE [cexp|$uint:(toInteger n)|]
compileConst (Word16C n)   = return $ ScalarCE [cexp|$uint:(toInteger n)|]
compileConst (Word32C n)   = return $ ScalarCE [cexp|$uint:(toInteger n)|]
compileConst (Word64C n)   = return $ ScalarCE [cexp|$ulint:(toInteger n)|]
compileConst (FloatC n)    = return $ ScalarCE [cexp|$float:(toRational n)|]
compileConst (DoubleC n)   = return $ ScalarCE [cexp|$double:(toRational n)|]

-- Compile an expression to a C expression
compileExp :: Exp -> C CExp
compileExp (VarE v)   = lookupVarTrans v
compileExp (ConstE c) = compileConst c
compileExp UnitE      = return VoidCE

compileExp (TupleE es) =
    TupCE <$> mapM compileExp es

compileExp (ProjE i _ e) = do
    ce  <- compileExp e
    tau <- inferExp e
    case ce of
      TupCE ces -> return $ ces !! i
      _ -> faildoc $ ppr e <+> text "::" <+> ppr tau <+> text "-->" <+> ppr ce

compileExp (LetE v tau _ e1 e2) = do
    ce1 <- bindExp (Just (unVar v)) e1
    extendVarTypes [(v, tau)] $ do
    extendVarTrans [(v, ce1)] $ do
    compileExp e2

compileExp (LamE vtaus e) = do
    dialect <- fromLJust fDialect <$> getFlags
    fname   <- gensym "f"
    tau_ret <- snd <$> (inferExp (LamE vtaus e) >>= checkFunT)
    ctx     <- getContext
    compileFun dialect ctx ctx fname vtaus tau_ret (compileExp e)

compileExp (AppE f es) = do
    dialect <- fromLJust fDialect <$> getFlags
    ctx     <- getContext
    tau     <- inferExp f
    cf      <- compileExp f
    compileCall dialect ctx ctx tau es $ \maybe_cresult cargs ->
        case maybe_cresult of
          Nothing      -> addStm [cstm|($cf)($args:cargs);|]
          Just cresult -> addStm [cstm|$cresult = ($cf)($args:cargs);|]

compileExp (CallE f es) =
    inContext Kernel $ do
    dialect           <- fromLJust fDialect <$> getFlags
    tau               <- inferExp f
    let (vtaus, body) =  splitLamE f
    callKernelFun dialect vtaus body tau es

compileExp (UnopE op e) = do
    tau <- inferExp e >>= checkScalarT
    ScalarCE <$> (go op tau <$> compileExp e)
  where
    go :: Unop -> ScalarType -> CExp -> C.Exp
    go (Cast Int8T)   _ ce = [cexp|(typename int8_t) $ce|]
    go (Cast Int16T)  _ ce = [cexp|(typename int16_t) $ce|]
    go (Cast Int32T)  _ ce = [cexp|(typename int32_t) $ce|]
    go (Cast Int64T)  _ ce = [cexp|(typename int64_t) $ce|]
    go (Cast Word8T)  _ ce = [cexp|(typename uint8_t) $ce|]
    go (Cast Word16T) _ ce = [cexp|(typename uint16_t) $ce|]
    go (Cast Word32T) _ ce = [cexp|(typename uint32_t) $ce|]
    go (Cast Word64T) _ ce = [cexp|(typename uint64_t) $ce|]
    go (Cast FloatT)  _ ce = [cexp|(float) $ce|]
    go (Cast DoubleT) _ ce = [cexp|(double) $ce|]

    go NotL _ ce = [cexp|!$ce|]

    go NegN _ ce = [cexp|-$ce|]

    go AbsN FloatT  ce                        = [cexp|fabsf($ce)|]
    go AbsN DoubleT ce                        = [cexp|fabs($ce)|]
    go AbsN tau     ce | isIntT (ScalarT tau) = [cexp|abs($ce)|]

    go SignumN FloatT  ce                        = [cexp|$ce > 0 ? 1 : ($ce < 0 ? -1 : 0)|]
    go SignumN DoubleT ce                        = [cexp|$ce > 0.0 ? 1.0 : ($ce < 0.0 ? -1.0 : 0.0)|]
    go SignumN tau     ce | isIntT (ScalarT tau) = [cexp|$ce > 0.0f ? 1.0f : ($ce < 0.0f ? -1.0f : 0.0f)|]

    go RecipF FloatT  ce = [cexp|1.0f/$ce|]
    go RecipF DoubleT ce = [cexp|1.0/$ce|]
    go ExpF   FloatT  ce = [cexp|expf($ce)|]
    go ExpF   DoubleT ce = [cexp|exp($ce)|]
    go SqrtF  FloatT  ce = [cexp|sqrtf($ce)|]
    go SqrtF  DoubleT ce = [cexp|sqrt($ce)|]
    go LogF   FloatT  ce = [cexp|logf($ce)|]
    go LogF   DoubleT ce = [cexp|log($ce)|]
    go SinF   FloatT  ce = [cexp|sinf($ce)|]
    go SinF   DoubleT ce = [cexp|sinf($ce)|]
    go TanF   FloatT  ce = [cexp|tanf($ce)|]
    go TanF   DoubleT ce = [cexp|tan($ce)|]
    go CosF   FloatT  ce = [cexp|cosf($ce)|]
    go CosF   DoubleT ce = [cexp|cos($ce)|]
    go AsinF  FloatT  ce = [cexp|asinf($ce)|]
    go AsinF  DoubleT ce = [cexp|asin($ce)|]
    go AtanF  FloatT  ce = [cexp|atanf($ce)|]
    go AtanF  DoubleT ce = [cexp|atan($ce)|]
    go AcosF  FloatT  ce = [cexp|acosf($ce)|]
    go AcosF  DoubleT ce = [cexp|acos($ce)|]
    go SinhF  FloatT  ce = [cexp|asinhf($ce)|]
    go SinhF  DoubleT ce = [cexp|asinh($ce)|]
    go TanhF  FloatT  ce = [cexp|atanhf($ce)|]
    go TanhF  DoubleT ce = [cexp|atanh($ce)|]
    go CoshF  FloatT  ce = [cexp|acoshf($ce)|]
    go CoshF  DoubleT ce = [cexp|acosh($ce)|]
    go AsinhF FloatT  ce = [cexp|asinhf($ce)|]
    go AsinhF DoubleT ce = [cexp|asinh($ce)|]
    go AtanhF FloatT  ce = [cexp|atanhf($ce)|]
    go AtanhF DoubleT ce = [cexp|atanh($ce)|]
    go AcoshF FloatT  ce = [cexp|acoshf($ce)|]
    go AcoshF DoubleT ce = [cexp|acosh($ce)|]

    go _ tau _ = errordoc $
                 text "Cannot compile" <+> ppr (UnopE op e) <+>
                 text "at type" <+> ppr tau

compileExp (BinopE op e1 e2) = do
    tau <- inferExp e1 >>= checkScalarT
    ScalarCE <$> (go op tau <$> compileExp e1 <*> compileExp e2)
  where
    go :: Binop -> ScalarType -> CExp -> CExp -> C.Exp
    go EqO _ ce1 ce2 = [cexp|$ce1 == $ce2|]
    go NeO _ ce1 ce2 = [cexp|$ce1 != $ce2|]
    go GtO _ ce1 ce2 = [cexp|$ce1 >  $ce2|]
    go GeO _ ce1 ce2 = [cexp|$ce1 >= $ce2|]
    go LtO _ ce1 ce2 = [cexp|$ce1 <  $ce2|]
    go LeO _ ce1 ce2 = [cexp|$ce1 <= $ce2|]

    go MaxO _ ce1 ce2 = [cexp|$ce1 > $ce2 ? $ce1 : $ce2 |]
    go MinO _ ce1 ce2 = [cexp|$ce1 > $ce2 ? $ce2 : $ce1 |]

    go AndL _ ce1 ce2 = [cexp|$ce1 && $ce2|]
    go OrL  _ ce1 ce2 = [cexp|$ce1 || $ce2|]

    go AddN _ ce1 ce2 = [cexp|$ce1 + $ce2|]
    go SubN _ ce1 ce2 = [cexp|$ce1 - $ce2|]
    go MulN _ ce1 ce2 = [cexp|$ce1 * $ce2|]
    go DivN _ ce1 ce2 = [cexp|$ce1 / $ce2|]

    go AndB _ ce1 ce2 = [cexp|$ce1 & $ce2|]
    go OrB  _ ce1 ce2 = [cexp|$ce1 | $ce2|]

    go ModI _ ce1 ce2 = [cexp|$ce1 % $ce2|]

    go PowF     FloatT  ce1 ce2 = [cexp|powf($ce1,$ce2)|]
    go PowF     DoubleT ce1 ce2 = [cexp|pow($ce1,$ce2)|]
    go LogBaseF FloatT  ce1 ce2 = [cexp|logf($ce2)/logf($ce1)|]
    go LogBaseF DoubleT ce1 ce2 = [cexp|log($ce2)/log($ce1)|]

    go _ tau _ _ = errordoc $
                   text "Cannot compile" <+> ppr (BinopE op e1 e2) <+>
                   text "at type" <+> ppr tau

compileExp (IfThenElseE test th el) = do
    tau      <- inferExp th
    ctest    <- compileExp test
    cvresult <- newCVar "ifte_result" tau
    cthitems <- inNewBlock_ $ do
                cresult <- compileExp th
                assignC cvresult cresult
    celitems <- inNewBlock_ $ do
                cresult <- compileExp el
                assignC cvresult cresult
    case celitems of
      [] -> addStm [cstm|if ($ctest) { $items:cthitems }|]
      _  -> addStm [cstm|if ($ctest) { $items:cthitems } else { $items:celitems }|]
    return cvresult

compileExp e@(SwitchE e_scrut cases dflt) = do
    tau      <- inferExp e
    cscrut   <- compileExp e_scrut
    cvresult <- newCVar "switch_result" tau
    ccases   <- (++) <$> mapM (compileCase cvresult) cases
                     <*> compileDefault cvresult dflt
    addStm [cstm|switch ($cscrut) { $stms:ccases }|]
    return cvresult
  where
    compileCase :: CExp -> (Int, Exp) -> C C.Stm
    compileCase cvresult (i, e) = do
        items <- inNewBlock_ $ do
                 ce <- compileExp e
                 assignC cvresult ce
        return [cstm|case $int:i: { $items:items }|]

    compileDefault :: CExp -> Maybe Exp -> C [C.Stm]
    compileDefault _ Nothing =
        return []

    compileDefault cvresult (Just e) = do
        items <- inNewBlock_ $ do
                 ce <- compileExp e
                 assignC cvresult ce
        return [[cstm|default: { $items:items }|]]

compileExp (ReturnE UnitE) =
    return VoidCE

compileExp (ReturnE e) =
    compileExp e

compileExp (SeqE m1 m2) = do
    compileExp m1
    compileExp m2

compileExp (ParE m1 m2) = do
    compileExp m1
    compileExp m2

compileExp (BindE v tau m1 m2) = do
    ce1 <- compileExp m1
    extendVarTypes [(v, tau)] $ do
    extendVarTrans [(v, ce1)] $ do
    compileExp m2

compileExp (AllocE tau_arr sh) = do
    dialect  <- fromLJust fDialect <$> getFlags
    (tau, _) <- checkArrayT tau_arr
    csh      <- mapM compileExp sh
    let csz  =  toSize csh
    cptr     <- allocPtr dialect csz tau
    return $ ArrayCE cptr csh
  where
    toSize :: [CExp] -> C.Exp
    toSize []       = [cexp|0|]
    toSize [ce]     = [cexp|$ce|]
    toSize (ce:ces) = [cexp|$ce*$(toSize ces)|]

    allocPtr :: Dialect -> C.Exp -> ScalarType -> C PtrCExp
    allocPtr dialect csz (TupleT taus) =
        TupPtrCE <$> mapM (allocPtr dialect csz) taus

    allocPtr dialect csz tau = do
        ctemp    <- gensym "alloc"
        let cptr =  PtrCE [cexp|$id:ctemp|]
        addLocal [cdecl|$ty:ctau $id:ctemp = NULL;|]
        case dialect of
          CUDA -> addStm [cstm|if(cudaMalloc(&$cptr, $exp:csz*sizeof($ty:ctau)) != cudaSuccess)
                                $stm:(failWithResult dialect cnegone) |]
          _    -> addStm [cstm|if(($cptr = ($ty:ctau) malloc($exp:csz*sizeof($ty:ctau))) == NULL)
                                $stm:(failWithResult dialect cnegone) |]
        addStm [cstm|allocs[nallocs] = (void*) $cptr;|]
        addStm [cstm|marks[nallocs++] = 0;|]
        return cptr
      where
        cnegone :: C.Exp
        cnegone = [cexp|-1|]

        ctau :: C.Type
        ctau = [cty|$ty:(toCType tau) *|]

compileExp (DimE i _ e) = do
    ce  <- compileExp e
    tau <- inferExp e
    case ce of
      ArrayCE _ sh -> return (sh !! i)
      _ -> faildoc $ ppr e <+> text "::" <+> ppr tau <+> text "-->" <+> ppr ce

compileExp (ProjArrE i _ e) = do
    ce  <- compileExp e
    tau <- inferExp e
    case ce of
      ArrayCE (TupPtrCE ptr) sh -> return $ ArrayCE (ptr !! i) sh
      _ -> faildoc $ ppr e <+> text "::" <+> ppr tau <+> text "-->" <+> ppr ce

compileExp (IndexE arr idx) = do
    carr <- compileExp arr
    cidx <- compileExp idx
    return $ ScalarCE [cexp|$carr[$cidx]|]

compileExp (WriteE arr idx e) = do
    carr <- compileExp arr
    cidx <- compileExp idx
    ce   <- compileExp e
    addStm [cstm|$carr[$cidx] = $ce;|]
    return VoidCE

compileExp (ForE isPar vs es m) = do
    dialect  <- fromLJust fDialect <$> getFlags
    tau      <- extendVarTypes (vs `zip` repeat ixT) $
                inferExp m
    cvresult <- newCVar "for_result" tau
    go (vs `zip` es) (allIdxs dialect) cvresult
    return cvresult
  where
    go :: [(Var, Exp)] -> [Idx] -> CExp -> C ()
    go _ [] _ =
        fail "compileFor: the impossible happened!"

    go [] _ cvresult = do
        cresult <- compileExp m
        assignC cvresult cresult

    go ((v@(Var i),bound):is) (idx:idxs) cresult = do
        dialect <- fromLJust fDialect <$> getFlags
        useIndex (idx,bound)
        let cv =  ScalarCE [cexp|$id:i|]
        cbound <- bindExp (Just "bound") bound
        extendVarTypes [(v, ixT)] $ do
        extendVarTrans [(v, cv)] $ do
        body <- inNewBlock_ $ go is idxs cresult
        when (isPar && dialect == OpenMP) $
            addStm [cstm|$pragma:("omp parallel for")|]
        addStm [cstm|for ($ty:(toCType ixT) $id:i = $(idxInit idx);
                          $id:i < $cbound;
                          $(idxStride idx i))
                     { $items:body }
                    |]

    allIdxs :: Dialect -> [Idx]
    allIdxs CUDA = map CudaThreadIdx [CudaDimX, CudaDimY, CudaDimZ] ++ repeat CIdx
    allIdxs _    = repeat CIdx

compileExp SyncE = do
    dialect <- fromLJust fDialect <$> getFlags
    case dialect of
      CUDA -> addStm [cstm|__syncthreads();|]
      _    -> return ()
    return VoidCE

compileExp e@(DelayedE {}) =
    faildoc $ text "Cannot compile:" <+> ppr e

-- | Compile a kernel function given a dialect. The result is a list of indices
-- and their bounds. A bound is represented as a function from the kernel's
-- arguments to an expression.
compileKernelFun :: Dialect -> String -> [(Var, Type)] -> Exp -> C [(Idx, [Exp] -> Exp)]
compileKernelFun dialect fname vtaus m =
    inContext Kernel $ do
    tau_kern <- inferExp (LamE vtaus m)
    tau_ret  <- snd <$> checkFunT tau_kern
    compileFun dialect Host Kernel fname vtaus tau_ret (compileExp m)
    idxs <- getIndices
    return [(idx, matchArgs vs bound) | (idx, bound) <- idxs]
  where
    vs = map fst vtaus

    -- Given a list of parameters, an expression written in terms of the parameters,
    -- and a list of arguments, @matchArgs@ rewrites the expression in terms of the
    -- arguments. We use this to take a loop bound, which occurs in the body of a
    -- kernel, and rewrite it to the equivalent expression in the caller's
    -- context. This allows the caller to work with the loop bounds and compute
    -- things like the proper CUDA grid and thread block parameters.
    matchArgs :: [Var]
              -> Exp
              -> [Exp]
              -> Exp
    matchArgs vs e args = runIdentity (go ExpA e)
      where
        go :: Traversal AST Identity
        go ExpA e@(VarE v) =
            case findIndex (== v) vs of
              Nothing -> Identity e
              Just i  -> Identity (args !! i)

        go w a = traverseFam go w a

-- | Call a kernel function (from the host).
callKernelFun :: Dialect
              -> [(Var, Type)]
              -> Exp
              -> Type
              -> [Exp]
              -> C CExp
callKernelFun dialect vtaus p tau es = do
    kern      <- gensym "kern"
    let cf    =  ScalarCE [cexp|$id:kern|]
    idxs      <- compileKernelFun dialect kern vtaus p
    let idxs' =  [(idx, f es) | (idx, f) <- idxs]
    compileCall dialect Host Kernel tau es (callKernel dialect cf idxs')
  where
    callKernel :: Dialect -> CExp -> [(Idx, Exp)] -> Maybe CExp -> [C.Exp] -> C ()
    callKernel CUDA cf idxs _ cargs = inBlock $ do
        let cudaIdxs = [(dim, boundsOf dim idxs) | dim <- [CudaDimX, CudaDimY, CudaDimZ]
                                                 , let bs = boundsOf dim idxs
                                                 , not (null bs)]
        addLocal [cdecl|typename dim3 gridDims;|]
        addLocal [cdecl|typename dim3 blockDims;|]
        mapM_ (setGridDim (cudaGridDims cudaIdxs)) [CudaDimX, CudaDimY, CudaDimZ]
        addStm [cstmCU|$cf<<<blockDims,gridDims>>>($args:cargs);|]

    callKernel _ cf _ Nothing cargs =
        addStm [cstm|$cf($args:cargs);|]

    callKernel _ cf _ (Just cresult) cargs =
        addStm [cstm|$cresult = $cf($args:cargs);|]

    setGridDim :: [(CudaDim, ([Exp], Exp, Exp))] -> CudaDim -> C ()
    setGridDim dims dim =
        case lookup dim dims of
          Nothing -> return ()
          Just (_, blockDim, gridDim) -> do cblockDim <- compileExp blockDim
                                            cgridDim  <- compileExp gridDim
                                            addStm [cstm|blockDims.$id:(cudaDimVar dim) = $cblockDim;|]
                                            addStm [cstm|gridDims.$id:(cudaDimVar dim)  = $cgridDim;|]

    cudaGridDims :: [(CudaDim, [Exp])] -> [(CudaDim, ([Exp], Exp, Exp))]
    cudaGridDims []            = []
    cudaGridDims [(dim, bs)]   = [(dim, (bs, 480, 128))]
    cudaGridDims [(dim1, bs1)
                 ,(dim2, bs2)] = [(dim1, (bs1, 16, 128))
                                 ,(dim2, (bs2,  8, 128))]
    cudaGridDims _             = error "cudaGridDims: failed to compute grid dimensions"

    boundsOf :: CudaDim -> [(Idx, Exp)] -> [Exp]
    boundsOf dim idxs = [e | (CudaThreadIdx dim', e) <- idxs, dim' == dim]

returnResultsByReference :: Dialect -> Context -> Context -> Type -> Bool
returnResultsByReference _ callerCtx calleeCtx tau
    | isUnitT tau                             = False
    | calleeCtx == callerCtx && isScalarT tau = False
    | otherwise                               = True

compileFun :: Dialect
           -> Context
           -> Context
           -> String
           -> [(Var, Type)]
           -> Type
           -> C CExp
           -> C CExp
compileFun dialect callerCtx calleeCtx fname vtaus tau_ret mbody = do
    (ps, body) <- inNewFunction $
                  extendParams vtaus $ do
                  cresult <- mbody
                  if returnResultsByReference dialect callerCtx calleeCtx tau_ret
                     then do  cvresult <- toCResultParam (dialect, callerCtx, calleeCtx) tau_ret
                              assignC cvresult cresult
                     else addStm [cstm|return $cresult;|]
    case (dialect, callerCtx, calleeCtx) of
      (CUDA, Host,   Host) ->
          addGlobal [cedeclCU|typename cudaError_t $id:fname($params:ps) { $items:body }|]
      (CUDA, Host,   Kernel) ->
          addGlobal [cedeclCU|extern "C" __global__ void $id:fname($params:ps) { $items:body }|]
      (CUDA, Kernel, Kernel) ->
          addGlobal [cedeclCU|__device__ $ty:ctau_ret $id:fname($params:ps) { $items:body }|]
      (_,    Host,   Host) ->
          addGlobal [cedecl|int $id:fname($params:ps) { $items:body }|]
      (_,    Host,   Kernel) ->
          addGlobal [cedecl|void $id:fname($params:ps) { $items:body }|]
      (_,    Kernel, Kernel) ->
          addGlobal [cedecl|$ty:ctau_ret $id:fname($params:ps) { $items:body } |]
      (_,    Kernel, Host) ->
          fail "Cannot call host function from device"
    return $ FunCE [cexp|$id:fname|]
  where
    ctau_ret :: C.Type
    ctau_ret = toCType tau_ret

compileCall :: Dialect                         -- ^ Dialect
            -> Context                         -- ^ Caller's context
            -> Context                         -- ^ Callee's context
            -> Type                            -- ^ The type of the function to call
            -> [Exp]                           -- ^ Function arguments
            -> (Maybe CExp -> [C.Exp] -> C ()) -- ^ Function to generate the
                                               -- call given a destination for
                                               -- the result of the call and the
                                               -- arguments to the call
            -> C CExp                          -- ^ Result of calling the function
compileCall dialect callerCtx calleeCtx tau args mcf = do
    tau_ret  <- snd <$> checkFunT tau
    cargs    <- concatMap toCArgs <$> mapM compileExp args
    let callCtx = (dialect, callerCtx, calleeCtx)
    case tau_ret of
      ScalarT UnitT -> do  mcf Nothing cargs
                           return VoidCE
      _ | calleeCtx == callerCtx && isScalarT tau_ret -> do
          cresult <- newCVar "call_result" tau_ret
          mcf (Just cresult) cargs
          return cresult
        | otherwise -> do
          toCResultArgs callCtx tau_ret $ \cresult cresultargs -> do
          mcf Nothing (cargs ++ cresultargs)
          return cresult

--
-- Result codes
--
declareResult :: Dialect -> C ()
declareResult CUDA = return ()
declareResult _    = addLocal [cdecl|int result = 0;|]

failWithResult :: Dialect -> C.Exp -> C.Stm
failWithResult CUDA _  = [cstm|goto done;|]
failWithResult _    ce = [cstm|{ result = $ce; goto done; }|]

returnResult :: Dialect -> C.Stm
returnResult CUDA = [cstm|return cudaGetLastError();|]
returnResult _    = [cstm|return result;|]

--
-- Memory allocation
--
declareHeap :: Dialect -> Int -> C ()
declareHeap dialect n = do
    addLocal [cdecl|void* allocs[$int:n];|]
    addLocal [cdecl|int   marks[$int:n];|]
    addLocal [cdecl|int   nallocs = 0;|]
    let free = case dialect of
                 CUDA -> [cstm|cudaFree((char*) allocs[i]);|]
                 _    -> [cstm|free(allocs[i]);|]
    addGlobal [cedecl|void gc(void **allocs, int* marks, int nallocs)
                      {
                        for (int i = 0; i < nallocs; ++i)
                        {
                          if (marks[i] == 0) {
                            $stm:free
                            allocs[i] = NULL;
                          }
                          marks[i] = 0;
                         }
                      }
                     |]
    addGlobal [cedecl|void mark(void **allocs, int* marks, int nallocs, void* alloc)
                      {
                        for (int i = 0; i < nallocs; ++i)
                        {
                          if (allocs[i] == alloc) {
                            marks[i] = 1;
                            return;
                          }
                         }
                      }
                     |]

gc :: C.Stm
gc = [cstm|gc(allocs, marks, nallocs);|]

mark :: CExp -> C ()
mark (VoidCE {}) =
    return ()

mark (ScalarCE {}) =
    return ()

mark (TupCE {}) =
    return ()

mark (ArrayCE ptr _) =
    go ptr
  where
    go :: PtrCExp -> C ()
    go (PtrCE ce) =
        addStm [cstm|mark(allocs, marks, nallocs, $ce);|]

    go (TupPtrCE ptrs) =
        mapM_ go ptrs

mark (FunCE _) =
    return ()

numAllocs :: Exp -> Int
numAllocs p =
    getSum (go ExpA p)
  where
    go :: Fold AST (Sum Int)
    go ExpA (AllocE (ArrayT tau _) _) = Sum (numScalarTs tau)
    go w    a                         = foldFam go w a

    numScalarTs :: ScalarType -> Int
    numScalarTs (TupleT taus) = sum (map numScalarTs taus)
    numScalarTs _             = 1

-- | Extend the current function's set of parameters
extendParams :: [(Var, Type)]
             -> C a
             -> C a
extendParams vtaus act = do
    cvs <- mapM toCParam vtaus
    extendVarTypes vtaus $ do
    extendVarTrans (vs `zip` cvs) $ do
    act
  where
    vs :: [Var]
    vs = map fst vtaus

-- | Conversion to C type
class IsCType a where
    toCType :: a -> C.Type

instance IsCType ScalarType where
    toCType UnitT       = [cty|void|]
    toCType BoolT       = [cty|typename uint8_t|]
    toCType Int8T       = [cty|typename int8_t|]
    toCType Int16T      = [cty|typename int16_t|]
    toCType Int32T      = [cty|typename int32_t|]
    toCType Int64T      = [cty|typename int64_t|]
    toCType Word8T      = [cty|typename uint8_t|]
    toCType Word16T     = [cty|typename uint16_t|]
    toCType Word32T     = [cty|typename uint32_t|]
    toCType Word64T     = [cty|typename uint64_t|]
    toCType FloatT      = [cty|float|]
    toCType DoubleT     = [cty|double|]
    toCType (TupleT {}) = error "toCType: cannot convert tuple type to C type"

instance IsCType PtrType where
    toCType (PtrT tau) = [cty|$ty:(toCType tau)*|]

instance IsCType Type where
    toCType (ScalarT tau) =
        toCType tau

    toCType (ArrayT (TupleT {}) _) =
        error "toCType: cannot convert array of tuple to C type"

    toCType (ArrayT tau _) =
        [cty|$ty:(toCType tau)*|]

    toCType (FunT taus tau) =
        [cty|$ty:(toCType tau) (*)($params:params)|]
      where
        -- XXX not quite right...
        params :: [C.Param]
        params = map (\tau -> [cparam|$ty:(toCType tau)|]) (concatMap flattenT taus)

    toCType (MT tau) =
        toCType tau

-- | C variable allocation
class NewCVar a where
    type CVar a :: *

    newCVar :: String -> a -> C (CVar a)

instance NewCVar ScalarType where
    type CVar ScalarType = CExp

    newCVar _ UnitT =
        return VoidCE

    newCVar v (TupleT taus) =
        TupCE <$> mapM (newCVar v) taus

    newCVar v tau = do
        ctemp <- gensym v
        addLocal [cdecl|$ty:(toCType tau) $id:ctemp;|]
        return $ ScalarCE [cexp|$id:ctemp|]

instance NewCVar PtrType where
    type CVar PtrType = PtrCExp

    newCVar _ (PtrT UnitT) =
        return $ PtrCE [cexp|NULL|]

    newCVar v (PtrT (TupleT taus)) =
        TupPtrCE <$> mapM (\tau -> newCVar v (PtrT tau)) taus

    newCVar v tau = do
        ctemp <- gensym v
        addLocal [cdecl|$ty:(toCType tau)* $id:ctemp;|]
        return $ PtrCE [cexp|$id:ctemp|]

instance NewCVar Type where
    type CVar Type = CExp

    newCVar v (ScalarT tau) =
        newCVar v tau

    newCVar v (ArrayT tau n) = do
        cptr  <- newCVar v (PtrT tau)
        cdims <- replicateM n (newCVar vdim ixScalarT)
        return $ ArrayCE cptr cdims
      where
        vdim :: String
        vdim = v ++ "dim"

    newCVar v tau@(FunT {}) = do
        ctemp <- gensym v
        addLocal [cdecl|$ty:(toCType tau) $id:ctemp;|]
        return $ FunCE [cexp|$id:ctemp|]

    newCVar v (MT tau) =
        newCVar v tau

-- | Type associated with a CExp thing
type family CExpType a :: *
type instance CExpType PtrCExp = PtrType
type instance CExpType CExp    = Type

-- | C assignment
class AssignC a where
    assignC :: a    -- ^ Destination
            -> a    -- ^ Source
            -> C ()

instance AssignC PtrCExp where
    assignC ce1@(PtrCE {}) ce2@(PtrCE {}) =
        addStm [cstm|$ce1 = $ce2;|]

    assignC (TupPtrCE ces1) (TupPtrCE ces2) | length ces1 == length ces2 =
        zipWithM_ assignC ces1 ces2

    assignC ce1 ce2 =
        faildoc $ text "assignC: cannot assign" <+> ppr ce2 <+>
                  text "to" <+> ppr ce1

instance AssignC CExp where
    assignC VoidCE VoidCE =
        return ()

    assignC ce1@(ScalarCE {}) ce2@(ScalarCE {}) =
        addStm [cstm|$ce1 = $ce2;|]

    assignC (TupCE ces1) (TupCE ces2) | length ces1 == length ces2 =
        zipWithM_ assignC ces1 ces2

    assignC (ArrayCE arr1 dims1) (ArrayCE arr2 dims2) | length dims1 == length dims2 = do
        assignC arr1 arr2
        zipWithM_ assignC dims1 dims2

    assignC (FunCE ce1) (FunCE ce2) =
        addStm [cstm|$ce1 = $ce2;|]

    assignC ce1 ce2 =
        faildoc $ text "assignC: cannot assign" <+> ppr ce2 <+>
                  text "to" <+> ppr ce1

-- | Convert an 'a' into function parameters
class IsCParam a where
    type CParam a :: *

    toCParam :: (Var, a) -> C (CParam a)

    toCResultParam :: (Dialect, Context, Context) -> a -> C (CParam a)

instance IsCParam ScalarType where
    type CParam ScalarType = CExp

    toCParam (_, UnitT) =
        return VoidCE

    toCParam (v, TupleT taus) =
        TupCE <$> mapM (\tau -> toCParam (v, tau)) taus

    toCParam (v, tau) = do
        ctemp <- gensym (unVar v)
        addParam [cparam|$ty:(toCType tau) $id:ctemp|]
        return $ ScalarCE [cexp|$id:ctemp|]

    toCResultParam _ UnitT =
        return VoidCE

    toCResultParam ctx (TupleT taus) =
        TupCE <$> mapM (toCResultParam ctx) taus

    toCResultParam (CUDA, Host, Kernel) tau = do
        ctemp <- gensym "cuscalar_resultparam"
        addParam [cparam|$ty:ctau* $id:ctemp|]
        return $ ScalarCE [cexp|*$id:ctemp|]
      where
        ctau :: C.Type
        ctau = toCType tau

    toCResultParam _ tau = do
        ctemp <- gensym "scalar_resultparam"
        addParam [cparam|$ty:(toCType tau)* $id:ctemp|]
        return $ ScalarCE [cexp|*$id:ctemp|]

instance IsCParam PtrType where
    type CParam PtrType = PtrCExp

    toCParam (_, PtrT UnitT) =
        return $ PtrCE [cexp|NULL|]

    toCParam (v, PtrT (TupleT taus)) =
        TupPtrCE <$> mapM (\tau -> toCParam (v, PtrT tau)) taus

    toCParam (v, PtrT tau) = do
        ctemp <- gensym (unVar v)
        addParam [cparam|$ty:(toCType tau)* $id:ctemp|]
        return $ PtrCE [cexp|$id:ctemp|]

    toCResultParam _ (PtrT UnitT) =
        return $ PtrCE [cexp|NULL|]

    toCResultParam ctx (PtrT (TupleT taus)) =
        TupPtrCE <$> mapM (toCResultParam ctx . PtrT) taus

    toCResultParam (CUDA, Host, Kernel) (PtrT tau) = do
        ctemp <- gensym "cuptr_resultparam"
        addParam [cparam|$ty:(toCType tau)** $id:ctemp|]
        return $ PtrCE [cexp|*$id:ctemp|]

    toCResultParam _ (PtrT tau) = do
        ctemp <- gensym "ptr_resultparam"
        addParam [cparam|$ty:(toCType tau)** $id:ctemp|]
        return $ PtrCE [cexp|*$id:ctemp|]

instance IsCParam Type where
    type CParam Type = CExp

    toCParam (v, ScalarT tau) =
        toCParam (v, tau)

    toCParam (v, ArrayT tau n) = do
        cptr  <- toCParam (v, PtrT tau)
        cdims <- replicateM n (toCParam (vdim, ixT))
        return $ ArrayCE cptr cdims
      where
        vdim :: Var
        vdim = Var (unVar v ++ "dim")

    toCParam (v, tau@(FunT {})) = do
        ctemp <- gensym (unVar v)
        addParam [cparam|$ty:(toCType tau) $id:ctemp|]
        return $ FunCE [cexp|$id:ctemp|]

    toCParam (v, MT tau) =
        toCParam (v, tau)

    toCResultParam ctx (ScalarT tau) =
        toCResultParam ctx tau

    toCResultParam ctx (ArrayT tau n) = do
        cptr  <- toCResultParam ctx (PtrT tau)
        cdims <- replicateM n (toCResultParam ctx ixT)
        return $ ArrayCE cptr cdims

    toCResultParam (CUDA, Host, Kernel) tau@(FunT {}) = do
        ctemp <- gensym "funresult_param"
        addParam [cparam|$ty:(toCType tau)* $id:ctemp|]
        return $ FunCE [cexp|*$id:ctemp|]

    toCResultParam _ tau@(FunT {}) = do
        ctemp <- gensym "funresult_param"
        addParam [cparam|$ty:(toCType tau)* $id:ctemp|]
        return $ FunCE [cexp|*$id:ctemp|]

    toCResultParam ctx (MT tau) =
        toCResultParam ctx tau

-- | Convert an 'a' into a list of C function arguments.
class IsCArg a where
    toCArgs :: a -> [C.Exp]

instance IsCArg PtrCExp where
    toCArgs (PtrCE ce)    = [[cexp|$ce|]]
    toCArgs (TupPtrCE es) = concatMap toCArgs es

instance IsCArg CExp where
    toCArgs VoidCE            = []
    toCArgs (ScalarCE ce)     = [[cexp|$ce|]]
    toCArgs (TupCE es)        = concatMap toCArgs es
    toCArgs (ArrayCE ce dims) = toCArgs ce ++ concatMap toCArgs dims
    toCArgs (FunCE ce)        = [ce]

-- | Convert an 'a' into a list of C function result arguments
class IsCResultArg a where
    type CResultArg a :: *

    toCResultArgs :: (Dialect, Context, Context)
                  -> a
                  -> (CResultArg a -> [C.Exp] -> C b)
                  -> C b

instance IsCResultArg a => IsCResultArg [a] where
    type CResultArg [a] = [CResultArg a]

    toCResultArgs ctx xs (kont :: [CResultArg a] -> [C.Exp] -> C b) =
        go [] [] xs
      where
        go :: [CResultArg a] -> [C.Exp] -> [a] -> C b
        go ces cargs [] =
            kont ces cargs

        go ces cargs (x:xs) =
            toCResultArgs ctx x $ \ces' cargs' ->
            go (ces ++ [ces']) (cargs ++ cargs') xs

instance IsCResultArg ScalarType where
    type CResultArg ScalarType = CExp

    toCResultArgs _ UnitT kont =
        kont VoidCE []

    toCResultArgs ctx (TupleT taus) kont =
        toCResultArgs ctx taus $ \ces cargs ->
        kont (TupCE ces) cargs

    toCResultArgs (CUDA, Host, Kernel) tau kont = do
        ce_h <- gensym "scalar_resultarg"
        ce_d <- gensym "cuscalar_resultarg"
        addLocal [cdecl|$ty:ctau  $id:ce_h;|]
        addLocal [cdecl|$ty:ctau* $id:ce_d;|]
        addStm [cstm|cudaMalloc(&$id:ce_d, sizeof($ty:ctau));|]
        x <- kont (ScalarCE [cexp|$id:ce_h|]) [[cexp|$id:ce_d|]]
        addStm [cstm|cudaMemcpy(&$id:ce_h,
                                $id:ce_d,
                                sizeof($ty:ctau),
                                cudaMemcpyDeviceToHost);|]
        addStm [cstm|cudaFree($id:ce_d);|]
        return x
      where
        ctau :: C.Type
        ctau = toCType tau

    toCResultArgs _ tau kont = do
        ce <- gensym "scalar_resultarg"
        addLocal [cdecl|$ty:(toCType tau) $id:ce;|]
        kont (ScalarCE [cexp|$id:ce|]) [[cexp|&$id:ce|]]

instance IsCResultArg PtrType where
    type CResultArg PtrType = PtrCExp

    toCResultArgs ctx (PtrT (TupleT taus)) kont =
        toCResultArgs ctx (map PtrT taus) $ \ces cargs ->
        kont (TupPtrCE ces) cargs

    toCResultArgs (CUDA, Host, Kernel) (PtrT tau) kont = do
        ce_h <- gensym "ptr_resultarg"
        ce_d <- gensym "cuptr_resultarg"
        addLocal [cdecl|$ty:ctau*  $id:ce_h = NULL;|]
        addLocal [cdecl|$ty:ctau** $id:ce_d = NULL;|]
        addStm [cstm|cudaMalloc(&$id:ce_d, sizeof($ty:ctau*));|]
        x <- kont (PtrCE [cexp|$id:ce_h|]) [[cexp|$id:ce_d|]]
        addStm [cstm|cudaMemcpy(&$id:ce_h,
                                $id:ce_d,
                                sizeof($ty:ctau*),
                                cudaMemcpyDeviceToHost);|]
        addStm [cstm|cudaFree($id:ce_d);|]
        return x
      where
        ctau :: C.Type
        ctau = toCType tau

    toCResultArgs _ (PtrT tau) kont = do
        ce <- gensym "ptr_resultarg"
        addLocal [cdecl|$ty:ctau* $id:ce = NULL;|]
        x <- kont (PtrCE [cexp|$id:ce|]) [[cexp|&$id:ce|]]
        return x
      where
        ctau :: C.Type
        ctau = toCType tau

instance IsCResultArg Type where
    type CResultArg Type = CExp

    toCResultArgs ctx (ScalarT tau) kont =
        toCResultArgs ctx tau kont

    toCResultArgs ctx (ArrayT tau n) kont =
        toCResultArgs ctx (PtrT tau)        $ \ce_ptr  cargs_ptr ->
        toCResultArgs ctx (replicate n ixT) $ \ces_dim cargs_dim ->
        kont (ArrayCE ce_ptr ces_dim) (cargs_ptr ++ cargs_dim)

    toCResultArgs _ tau@(FunT {}) kont = do
        ce_h <- gensym "fun_resultarg"
        ce_d <- gensym "cufun_resultarg"
        addLocal [cdecl|$ty:ctau*  $id:ce_h = NULL;|]
        addLocal [cdecl|$ty:ctau** $id:ce_d = NULL;|]
        addStm [cstm|cudaMalloc(&$id:ce_d, sizeof($ty:ctau*));|]
        x <- kont (FunCE [cexp|$id:ce_h|]) [[cexp|$id:ce_h|]]
        addStm [cstm|cudaMemcpy(&$id:ce_h,
                                $id:ce_d,
                                sizeof($ty:ctau*),
                                cudaMemcpyDeviceToHost);|]
        addStm [cstm|cudaFree($id:ce_d);|]
        return x
      where
        ctau :: C.Type
        ctau = toCType tau

    toCResultArgs ctx (MT tau) kont =
        toCResultArgs ctx tau kont

--
-- Expression binding
--
bindExp :: Maybe String -> Exp -> C CExp
bindExp maybe_v e = do
    ce <- compileExp e
    if isAtomic ce
      then return ce
      else do tau <- inferExp e
              cv  <- newCVar (maybe "temp" (++ "_") maybe_v) tau
              assignC cv ce
              return cv

isAtomic :: CExp -> Bool
isAtomic (ScalarCE (C.Var {}))   = True
isAtomic (ScalarCE (C.Const {})) = True
isAtomic (FunCE (C.Var {}))      = True
isAtomic _                       = False
