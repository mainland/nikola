{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
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
    compileKernelProc
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

compileProgram :: Flags -> ProcH -> IO [C.Definition]
compileProgram flags p = do
    snd <$> runC flags go
  where
    go :: C CExp
    go = do
        case fromLJust fDialect flags of
          CUDA   -> do  addInclude "\"cuda.h\""
                        addInclude "\"cuda_runtime_api.h\""
          OpenMP -> do  addInclude "<stdlib.h>"
                        addInclude "<inttypes.h>"
                        addInclude "<math.h>"
                        addInclude "<omp.h>"
          _      -> return ()
        compileHostProc p

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

compileExp (ProjArrE i _ e) = do
    ce  <- compileExp e
    tau <- inferExp e
    case ce of
      ArrayCE (TupPtrCE ptr) sh -> return $ ArrayCE (ptr !! i) sh
      _ -> faildoc $ ppr e <+> text "::" <+> ppr tau <+> text "-->" <+> ppr ce

compileExp (DimE i _ e) = do
    ce  <- compileExp e
    tau <- inferExp e
    case ce of
      ArrayCE _ sh -> return (sh !! i)
      _ -> faildoc $ ppr e <+> text "::" <+> ppr tau <+> text "-->" <+> ppr ce

compileExp (LetE v tau _ e1 e2) = do
    ce1 <- bindExp (Just (unVar v)) e1
    extendVarTypes [(v, tau)] $ do
    extendVarTrans [(v, ce1)] $ do
    compileExp e2

compileExp (LamE vtaus e) = do
    dialect <- fromLJust fDialect <$> getFlags
    fname   <- gensym "f"
    tau_ret <- snd <$> (inferExp (LamE vtaus e) >>= inferFunT)
    ctx     <- getContext
    compileFun dialect (Fun ctx) fname vtaus tau_ret (compileExp e)

compileExp (AppE f es) = do
    dialect <- fromLJust fDialect <$> getFlags
    ctx     <- getContext
    tau     <- inferExp f
    cf      <- compileExp f
    compileCall dialect (Fun ctx) tau es $ \cargs -> do
        return $ ScalarCE [cexp|($cf)($args:cargs)|]

compileExp (UnopE op e) = do
    ScalarCE <$> (go op <$> compileExp e)
  where
    go :: Unop -> CExp -> C.Exp
    go Lnot e = [cexp|!$e|]

    go Ineg e    = [cexp|-$e|]
    go Iabs e    = [cexp|abs($e)|]
    go Isignum e = [cexp|$e > 0 ? 1 : ($e < 0 ? -1 : 0)|]

    go Itof e    = [cexp|(float) $e|]
    go Itod e    = [cexp|(double) $e|]

    go Fneg e    = [cexp|-$e|]
    go Fabs e    = [cexp|fabsf($e)|]
    go Fsignum e = [cexp|$e > 0 ? 1 : ($e < 0 ? -1 : 0)|]
    go Frecip e  = [cexp|1.0f/$e|]

    go Fexp e   = [cexp|expf($e)|]
    go Fsqrt e  = [cexp|sqrtf($e)|]
    go Flog e   = [cexp|logf($e)|]
    go Fsin e   = [cexp|sinf($e)|]
    go Ftan e   = [cexp|tanf($e)|]
    go Fcos e   = [cexp|cosf($e)|]
    go Fasin e  = [cexp|asinf($e)|]
    go Fatan e  = [cexp|atanf($e)|]
    go Facos e  = [cexp|acosf($e)|]
    go Fsinh e  = [cexp|asinhf($e)|]
    go Ftanh e  = [cexp|atanhf($e)|]
    go Fcosh e  = [cexp|acoshf($e)|]
    go Fasinh e = [cexp|asinhf($e)|]
    go Fatanh e = [cexp|atanhf($e)|]
    go Facosh e = [cexp|acoshf($e)|]

    go Dneg e    = [cexp|-$e|]
    go Dabs e    = [cexp|fabs($e)|]
    go Dsignum e = [cexp|$e > 0 ? 1 : ($e < 0 ? -1 : 0)|]
    go Drecip e  = [cexp|1.0/$e|]

    go Dexp e   = [cexp|exp($e)|]
    go Dsqrt e  = [cexp|sqrt($e)|]
    go Dlog e   = [cexp|log($e)|]
    go Dsin e   = [cexp|sin($e)|]
    go Dtan e   = [cexp|tan($e)|]
    go Dcos e   = [cexp|cos($e)|]
    go Dasin e  = [cexp|asin($e)|]
    go Datan e  = [cexp|atan($e)|]
    go Dacos e  = [cexp|acos($e)|]
    go Dsinh e  = [cexp|asinh($e)|]
    go Dtanh e  = [cexp|atanh($e)|]
    go Dcosh e  = [cexp|acosh($e)|]
    go Dasinh e = [cexp|asinh($e)|]
    go Datanh e = [cexp|atanh($e)|]
    go Dacosh e = [cexp|acosh($e)|]

compileExp (BinopE op e1 e2) =
    ScalarCE <$> (go op <$> compileExp e1 <*> compileExp e2)
  where
    go :: Binop -> CExp -> CExp -> C.Exp
    go Land e1 e2 = [cexp|$e1 && $e2|]
    go Lor  e1 e2 = [cexp|$e1 || $e2|]

    go Leq e1 e2 = [cexp|$e1 == $e2|]
    go Lne e1 e2 = [cexp|$e1 != $e2|]
    go Lgt e1 e2 = [cexp|$e1 > $e2|]
    go Lge e1 e2 = [cexp|$e1 >= $e2|]
    go Llt e1 e2 = [cexp|$e1 < $e2|]
    go Lle e1 e2 = [cexp|$e1 <= $e2|]

    go Band e1 e2 = [cexp|$e1 & $e2|]
    go Bor  e1 e2 = [cexp|$e1 | $e2|]

    go Bmax e1 e2 = [cexp|$e1 > $e2 ? $e1 : $e2 |]
    go Bmin e1 e2 = [cexp|$e1 > $e2 ? $e2 : $e1 |]

    go Iadd e1 e2 = [cexp|$e1 + $e2|]
    go Isub e1 e2 = [cexp|$e1 - $e2|]
    go Imul e1 e2 = [cexp|$e1 * $e2|]
    go Idiv e1 e2 = [cexp|$e1 / $e2|]
    go Imod e1 e2 = [cexp|$e1 % $e2|]

    go Fadd e1 e2 = [cexp|$e1 + $e2|]
    go Fsub e1 e2 = [cexp|$e1 - $e2|]
    go Fmul e1 e2 = [cexp|$e1 * $e2|]
    go Fdiv e1 e2 = [cexp|$e1 / $e2|]

    go Fpow e1 e2     = [cexp|powf($e1,$e2)|]
    go FlogBase e1 e2 = [cexp|logf($e2)/logf($e1)|]

    go Dadd e1 e2 = [cexp|$e1 + $e2|]
    go Dsub e1 e2 = [cexp|$e1 - $e2|]
    go Dmul e1 e2 = [cexp|$e1 * $e2|]
    go Ddiv e1 e2 = [cexp|$e1 / $e2|]

    go Dpow e1 e2     = [cexp|pow($e1,$e2)|]
    go DlogBase e1 e2 = [cexp|log($e2)/log($e1)|]

compileExp (IfThenElseE test th el) = do
    tau <- inferExp th
    compileIfThenElse test th el tau compileExp

compileExp e@(SwitchE e_scrut cases dflt) = do
    tau      <- inferExp e
    cscrut   <- compileExp e_scrut
    cvresult <- newCVar "result" tau
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

compileExp (IndexE arr idx) = do
    carr <- compileExp arr
    cidx <- compileExp idx
    return $ ScalarCE [cexp|$carr[$cidx]|]

compileExp e@(DelayedE {}) =
    faildoc $ text "Cannot compile:" <+> ppr e

compileHost :: ProgH -> C CExp
compileHost (ReturnH UnitE) =
    return VoidCE

compileHost (ReturnH e) = do
    ce <- compileExp e
    return ce

compileHost (SeqH m1 m2) = do
    compileHost m1
    compileHost m2

compileHost (LetH v tau e m) = do
    ce1 <- compileExp e
    extendVarTypes [(v, tau)] $ do
    extendVarTrans [(v, ce1)] $ do
    compileHost m

compileHost (BindH v tau m1 m2) = do
    ce1 <- compileHost m1
    extendVarTypes [(v, tau)] $ do
    extendVarTrans [(v, ce1)] $ do
    compileHost m2

compileHost (LiftH k es) = do
    dialect <- fromLJust fDialect <$> getFlags
    tau_k   <- inferProcK k
    callKernelProc dialect k tau_k es

compileHost (IfThenElseH test th el) = do
    tau <- inferProgH th
    compileIfThenElse test th el tau compileHost

compileHost (AllocH tau_arr sh) = do
    dialect  <- fromLJust fDialect <$> getFlags
    (tau, _) <- inferArrayT tau_arr
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

compileHost m@(DelayedH {}) =
    faildoc $ text "Cannot compile:" <+> ppr m

compileHostProc :: ProcH -> C CExp
compileHostProc p@(ProcH vtaus m) = do
    flags       <- getFlags
    let dialect =  fromLJust fDialect flags
    fname       <- case fFunction flags of
                     Last Nothing      -> gensym "host"
                     Last (Just fname) -> return fname
    tau_host    <- inferProcH p
    tau_ret     <- snd <$> inferFunT tau_host
    compileFun dialect (Proc Host) fname vtaus tau_ret $ do
    declareHeap dialect (numAllocs p)
    declareResult dialect
    addFinalStm (returnResult dialect)
    addFinalStm [cstm|done: $stm:gc|]
    ce <- compileHost m
    mark ce
    return ce

compileKernel :: ProgK -> C CExp
compileKernel (ReturnK UnitE) =
    return VoidCE

compileKernel (ReturnK e) = do
    compileExp e

compileKernel (SeqK m1 m2) = do
    compileKernel m1
    compileKernel m2

compileKernel (ParK m1 m2) = do
    compileKernel m1
    compileKernel m2

compileKernel (LetK v tau e m) = do
    cv <- newCVar (unVar v) tau
    ce <- compileExp e
    assignC cv ce
    extendVarTypes [(v, tau)] $ do
    extendVarTrans [(v, cv)] $ do
    compileKernel m

compileKernel (BindK v tau m1 m2) = do
    cv  <- newCVar (unVar v) tau
    ce1 <- compileKernel m1
    assignC cv ce1
    extendVarTypes [(v, tau)] $ do
    extendVarTrans [(v, cv)] $ do
    compileKernel m2

compileKernel (ForK vs es m) = do
    dialect <- fromLJust fDialect <$> getFlags
    compileFor dialect False (vs `zip` es) m

compileKernel (ParforK vs es m) = do
    dialect <- fromLJust fDialect <$> getFlags
    compileFor dialect True (vs `zip` es) m

compileKernel (IfThenElseK test th el) = do
    tau <- inferProgK th
    compileIfThenElse test th el tau compileKernel

compileKernel (WriteK arr idx e) = do
    carr <- compileExp arr
    cidx <- compileExp idx
    ce   <- compileExp e
    addStm [cstm|$carr[$cidx] = $ce;|]
    return VoidCE

compileKernel SyncK = do
    dialect <- fromLJust fDialect <$> getFlags
    case dialect of
      CUDA -> addStm [cstm|__syncthreads();|]
      _    -> return ()
    return VoidCE

compileKernel m =
    faildoc $ text "Cannot compile:" <+> ppr m

-- Compile a kernel procedure given a dialect. The result is a function and a
-- list of indices and their bounds. A bound is represented as a function from
-- the kernel's arguments to an expression.
compileKernelProc :: Dialect -> String -> ProcK -> C [(Idx, [Exp] -> Exp)]
compileKernelProc dialect fname p@(ProcK vtaus m) =
    inContext Kernel $ do
    tau_kern <- inferProcK p
    tau_ret  <-snd <$> inferFunT tau_kern
    compileFun dialect (Proc Kernel) fname vtaus tau_ret (compileKernel m)
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

-- Compile a for or parallel for loop
compileFor :: Dialect -> Bool -> [(Var, Exp)] -> ProgK -> C CExp
compileFor dialect isParallel bounds m = do
    tau      <- extendVarTypes (map fst bounds `zip` repeat ixT) $
                inferProgK m
    cvresult <- newCVar "result" tau
    go bounds (allIdxs dialect) cvresult
    return cvresult
  where
    go :: [(Var, Exp)] -> [Idx] -> CExp -> C ()
    go _ [] _ =
        fail "compileFor: the impossible happened!"

    go [] _ cvresult = do
        cresult <- compileKernel m
        assignC cvresult cresult

    go ((v@(Var i),bound):is) (idx:idxs) cresult = do
        useIndex (idx,bound)
        let cv =  ScalarCE [cexp|$id:i|]
        cbound <- bindExp (Just "bound") bound
        extendVarTypes [(v, ixT)] $ do
        extendVarTrans [(v, cv)] $ do
        body <- inNewBlock_ $ go is idxs cresult
        when (isParallel && dialect == OpenMP) $
            addStm [cstm|$pragma:("omp parallel for")|]
        addStm [cstm|for ($ty:(toCType ixT) $id:i = $(idxInit idx);
                          $id:i < $cbound;
                          $(idxStride idx i))
                     { $items:body }
                    |]

    allIdxs :: Dialect -> [Idx]
    allIdxs CUDA = map CudaThreadIdx [CudaDimX, CudaDimY, CudaDimZ] ++ repeat CIdx
    allIdxs _    = repeat CIdx

-- Compile an if/then/else
compileIfThenElse :: Exp -> a -> a
                  -> Type
                  -> (a -> C CExp)
                  -> C CExp
compileIfThenElse test th el tau compile = do
    ctest    <- compileExp test
    cvresult <- newCVar "result" tau
    cthitems <- inNewBlock_ $ do
                cresult <- compile th
                assignC cvresult cresult
    celitems <- inNewBlock_ $ do
                cresult <- compile el
                assignC cvresult cresult
    case celitems of
      [] -> addStm [cstm|if ($ctest) { $items:cthitems }|]
      _  -> addStm [cstm|if ($ctest) { $items:cthitems } else { $items:celitems }|]
    return cvresult

-- Call a kernel procedure
callKernelProc :: Dialect
               -> ProcK
               -> Type
               -> [Exp]
               -> C CExp
callKernelProc dialect p tau es = do
    kern      <- gensym "kern"
    let cf    =  ScalarCE [cexp|$id:kern|]
    idxs      <- compileKernelProc dialect kern p
    let idxs' =  [(idx, f es) | (idx, f) <- idxs]
    inBlock $ compileCall dialect (Proc Kernel) tau es (callKernel dialect cf idxs')
  where
    callKernel :: Dialect -> CExp -> [(Idx, Exp)] -> [C.Exp] -> C CExp
    callKernel CUDA cf idxs cargs = do
        let cudaIdxs = [(dim, boundsOf dim idxs) | dim <- [CudaDimX, CudaDimY, CudaDimZ]
                                                 , let bs = boundsOf dim idxs
                                                 , not (null bs)]
        addLocal [cdecl|typename dim3 gridDims;|]
        addLocal [cdecl|typename dim3 blockDims;|]
        mapM_ (setGridDim (cudaGridDims cudaIdxs)) [CudaDimX, CudaDimY, CudaDimZ]
        return $ ScalarCE [cexpCU|$cf<<<blockDims,gridDims>>>($args:cargs)|]

    callKernel _ cf _ cargs =
        return $ ScalarCE [cexpCU|$cf($args:cargs)|]

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

data FunSort = Proc Context
             | Fun Context

compileFun :: Dialect
           -> FunSort
           -> String
           -> [(Var, Type)]
           -> Type
           -> C CExp
           -> C CExp
compileFun dialect sort fname vtaus tau_ret mbody = do
    (ps, body) <- inNewFunction $
                  extendParams vtaus $ do
                  cresult <- mbody
                  if returnResultsByReference
                     then do  cvresult <- toCResultParam tau_ret
                              assignC cvresult cresult
                     else addStm [cstm|return $cresult;|]
    case (dialect, sort) of
      (CUDA, Proc Host) ->
          addGlobal [cedeclCU|typename cudaError_t $id:fname($params:ps) { $items:body }|]
      (_,    Proc Host) ->
          addGlobal [cedecl|int $id:fname($params:ps) { $items:body }|]
      (CUDA, Proc Kernel) ->
          addGlobal [cedeclCU|extern "C" __global__ void $id:fname($params:ps) { $items:body }|]
      (_,    Proc Kernel) ->
          addGlobal [cedecl|void $id:fname($params:ps) { $items:body }|]
      (CUDA, Fun Kernel) ->
          addGlobal [cedeclCU|__device__ $ty:ctau_ret $id:fname($params:ps) { $items:body }|]
      (_,    Fun _) ->
          addGlobal [cedecl|$ty:ctau_ret $id:fname($params:ps) { $items:body } |]
    return $ ScalarCE [cexp|$id:fname|]
  where
    ctau_ret :: C.Type
    ctau_ret = toCType tau_ret

    returnResultsByReference :: Bool
    returnResultsByReference =
        case (dialect, sort, tau_ret) of
          (_, Fun _, tau) | isScalarT tau -> False
          _                               -> True

compileCall :: Dialect             -- ^ Dialect
            -> FunSort             -- ^ Function sort
            -> Type                -- ^ The type of the function to call
            -> [Exp]               -- ^ Function arguments
            -> ([C.Exp] -> C CExp) -- ^ Function to generate the call given its
                                   -- arguments
            -> C CExp              -- ^ Result of calling the function
compileCall dialect sort tau args mcf = do
    tau_ret  <- snd <$> inferFunT tau
    cargs    <- concatMap toCArgs <$> mapM compileExp args
    cvresult <- newCVar "result" tau_ret
    case (dialect, sort, tau_ret) of
      (CUDA, Proc Kernel, _) ->
          do  cudaresult <- toCUDAResultParam tau_ret
              ccall      <- mcf (cargs ++ toCArgs cudaresult)
              addStm [cstm|$ccall;|]
              assignCUDAResult tau_ret cvresult cudaresult
      (_, Fun _, ScalarT UnitT) ->
          do  ccall <- mcf cargs
              addStm [cstm|$ccall;|]
      (_, Fun _, tau) | isScalarT tau ->
          do  ccall <- mcf cargs
              addStm [cstm|$cvresult = $ccall;|]
      (_, _, _) ->
          do  ccall <- mcf (cargs ++ toCResultArgs cvresult)
              addStm [cstm|$ccall;|]
    return cvresult

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

numAllocs :: ProcH -> Int
numAllocs p =
    getSum (go ProcHA p)
  where
    go :: Fold AST (Sum Int)
    go ProgHA (AllocH (ArrayT tau _) _) = Sum (numScalarTs tau)
    go w      a                         = foldFam go w a

    numScalarTs :: ScalarType -> Int
    numScalarTs (TupleT taus) = sum (map numScalarTs taus)
    numScalarTs _             = 1

-- | Extend the current function's set of parameters
extendParams :: [(Var, Type)] -> C a -> C a
extendParams vtaus act = do
    cvs <- mapM toCParam vtaus
    extendVarTypes vtaus $ do
    extendVarTrans (vs `zip` cvs) $ do
    act
  where
    vs :: [Var]
    vs = map fst vtaus

-- | Type associated with a CExp thing
type family CExpType a :: *
type instance CExpType PtrCExp    = PtrType
type instance CExpType CExp       = Type

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
        addParam [cparam|$ty:(toCType tau)* $id:ctemp|]
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
        addParam [cparam|$ty:(toCType tau) $id:ctemp|]
        return $ FunCE [cexp|$id:ctemp|]

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

-- | C assignment
class AssignCUDAResult a where
    assignCUDAResult :: CExpType a
                     -> a    -- ^ Destination
                     -> a    -- ^ Source
                     -> C ()

instance AssignCUDAResult PtrCExp where
    assignCUDAResult tau ce1@(PtrCE {}) ce2@(PtrCE {}) = do
        addStm [cstm|cudaMemcpy(&$ce1,
                                $ce2,
                                sizeof($ty:(toCType tau)),
                                cudaMemcpyDeviceToHost);|]
        addStm [cstm|cudaFree($ce2);|]

    assignCUDAResult (PtrT (TupleT taus)) (TupPtrCE ces1) (TupPtrCE ces2)
        |  length ces1 == length taus && length ces2 == length taus =
        zipWith3M_ assignCUDAResult (map PtrT taus) ces1 ces2

    assignCUDAResult _ ce1 ce2 =
        faildoc $ text "assignCUDAResult: cannot assign" <+> ppr ce2 <+>
                  text "to" <+> ppr ce1

instance AssignCUDAResult CExp where
    assignCUDAResult _ VoidCE VoidCE =
        return ()

    assignCUDAResult tau ce1@(ScalarCE {}) ce2@(ScalarCE {}) = do
        addStm [cstm|cudaMemcpy(&$ce1,
                                $ce2,
                                sizeof($ty:(toCType tau)),
                                cudaMemcpyDeviceToHost);|]
        addStm [cstm|cudaFree($ce2);|]

    assignCUDAResult (ScalarT (TupleT taus)) (TupCE ces1) (TupCE ces2)
        | length ces1 == length taus && length ces2 == length taus =
        zipWith3M_ assignCUDAResult (map ScalarT taus) ces1 ces2

    assignCUDAResult (ArrayT tau n) (ArrayCE arr1 dims1) (ArrayCE arr2 dims2)
        | length dims1 == n && length dims2 == n = do
        assignCUDAResult (PtrT tau) arr1 arr2
        zipWithM_ (assignCUDAResult ixT) dims1 dims2

    assignCUDAResult tau@(FunT {}) (FunCE ce1) (FunCE ce2) = do
        addStm [cstm|cudaMemcpy(&$ce1,
                                $ce2,
                                sizeof($ty:(toCType tau)),
                                cudaMemcpyDeviceToHost);|]
        addStm [cstm|cudaFree($ce2);|]

    assignCUDAResult _ ce1 ce2 =
        faildoc $ text "assignCUDAResult: cannot assign" <+> ppr ce2 <+>
                  text "to" <+> ppr ce1

-- | Convert an 'a' into a C type
class IsCType a where
    toCType :: a -> C.Type

instance IsCType ScalarType where
    toCType UnitT           = [cty|void|]
    toCType BoolT           = [cty|typename uint8_t|]
    toCType Int8T           = [cty|typename int8_t|]
    toCType Int16T          = [cty|typename int16_t|]
    toCType Int32T          = [cty|typename int32_t|]
    toCType Int64T          = [cty|typename int64_t|]
    toCType Word8T          = [cty|typename uint8_t|]
    toCType Word16T         = [cty|typename uint16_t|]
    toCType Word32T         = [cty|typename uint32_t|]
    toCType Word64T         = [cty|typename uint64_t|]
    toCType FloatT          = [cty|float|]
    toCType DoubleT         = [cty|double|]
    toCType (TupleT {})     = error "toCType: cannot convert tuple type to C types"

instance IsCType PtrType where
    toCType (PtrT tau) = [cty|$ty:(toCType tau)*|]

instance IsCType Type where
    toCType (ScalarT tau) =
        toCType tau

    toCType (ArrayT (TupleT {}) _) =
        error "toCType: cannot convert array of tuple type to C types"

    toCType (ArrayT tau _) =
        [cty|$ty:(toCType tau)*|]

    toCType (FunT taus tau) =
        [cty|$ty:(toCType tau) (*)($params:params)|]
      where
        -- XXX not quite right...
        params :: [C.Param]
        params = map (\tau -> [cparam|$ty:(toCType tau)|]) (concatMap flattenT taus)

-- | Convert an 'a' into function parameters
class IsCParam a where
    type CParam a :: *

    toCParam          :: (Var, a) -> C (CParam a)
    toCResultParam    :: a        -> C (CParam a)
    toCUDAResultParam :: a        -> C (CParam a)

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

    toCResultParam UnitT =
        return VoidCE

    toCResultParam (TupleT taus) =
        TupCE <$> mapM toCResultParam taus

    toCResultParam tau = do
        ctemp <- gensym "result"
        addParam [cparam|$ty:(toCType tau)* $id:ctemp|]
        return $ ScalarCE [cexp|*$id:ctemp|]

    toCUDAResultParam UnitT =
        return VoidCE

    toCUDAResultParam (TupleT taus) =
        TupCE <$> mapM toCUDAResultParam taus

    toCUDAResultParam tau = do
        ctemp <- gensym "result"
        addLocal [cdecl|$ty:ctau* $id:ctemp;|]
        addStm [cstm|cudaMalloc(&$id:ctemp, sizeof($ty:ctau));|]
        return $ ScalarCE [cexp|$id:ctemp|]
      where
        ctau :: C.Type
        ctau = toCType tau

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

    toCResultParam (PtrT UnitT) =
        return $ PtrCE [cexp|NULL|]

    toCResultParam (PtrT (TupleT taus)) =
        TupPtrCE <$> mapM (toCResultParam . PtrT) taus

    toCResultParam (PtrT tau) = do
        ctemp <- gensym "result"
        addParam [cparam|$ty:(toCType tau)** $id:ctemp|]
        return $ PtrCE [cexp|*$id:ctemp|]

    toCUDAResultParam (PtrT UnitT) =
        return $ PtrCE [cexp|NULL|]

    toCUDAResultParam (PtrT (TupleT taus)) =
        TupPtrCE <$> mapM (toCUDAResultParam . PtrT) taus

    toCUDAResultParam (PtrT tau) = do
        ctemp <- gensym "result"
        addParam [cparam|$ty:(toCType tau)** $id:ctemp|]
        addStm [cstm|cudaMalloc(&$id:ctemp, sizeof($ty:(ctau)*));|]
        return $ PtrCE [cexp|$id:ctemp|]
      where
        ctau :: C.Type
        ctau = toCType tau

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

    toCResultParam (ScalarT tau) =
        toCResultParam tau

    toCResultParam (ArrayT tau n) = do
        cptr  <- toCResultParam (PtrT tau)
        cdims <- replicateM n (toCResultParam ixT)
        return $ ArrayCE cptr cdims

    toCResultParam tau@(FunT {}) = do
        ctemp <- gensym "f"
        addParam [cparam|$ty:(toCType tau)* $id:ctemp|]
        return $ FunCE [cexp|*$id:ctemp|]

    toCUDAResultParam (ScalarT tau) =
        toCUDAResultParam tau

    toCUDAResultParam (ArrayT tau n) = do
        cptr  <- toCUDAResultParam (PtrT tau)
        cdims <- replicateM n (toCUDAResultParam ixScalarT)
        return $ ArrayCE cptr cdims

    toCUDAResultParam tau@(FunT {}) = do
        ctemp <- gensym "f"
        addParam [cparam|$ty:(toCType tau)* $id:ctemp|]
        addStm [cstm|cudaMalloc(&$id:ctemp, sizeof($ty:ctau));|]
        return $ FunCE [cexp|$id:ctemp|]
      where
        ctau :: C.Type
        ctau = toCType tau

-- | Convert an 'a' into a list of C function arguments.
class IsCArg a where
    toCArgs       :: a -> [C.Exp]
    toCResultArgs :: a -> [C.Exp]

instance IsCArg PtrCExp where
    toCArgs (PtrCE ce)    = [[cexp|$ce|]]
    toCArgs (TupPtrCE es) = concatMap toCArgs es

    toCResultArgs (PtrCE ce)    = [[cexp|&$ce|]]
    toCResultArgs (TupPtrCE es) = concatMap toCResultArgs es

instance IsCArg CExp where
    toCArgs VoidCE            = []
    toCArgs (ScalarCE ce)     = [[cexp|$ce|]]
    toCArgs (TupCE es)        = concatMap toCArgs es
    toCArgs (ArrayCE ce dims) = toCArgs ce ++ concatMap toCArgs dims
    toCArgs (FunCE ce)        = [ce]

    toCResultArgs VoidCE            = []
    toCResultArgs (ScalarCE ce)     = [[cexp|&$ce|]]
    toCResultArgs (TupCE es)        = concatMap toCResultArgs es
    toCResultArgs (ArrayCE ce dims) = toCResultArgs ce ++ concatMap toCResultArgs dims
    toCResultArgs (FunCE ce)        = [[cexp|&$ce|]]

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
isAtomic _                       = False

zipWith3M_ :: (Monad m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m ()
zipWith3M_ f xs ys zs =  sequence_ (zipWith3 f xs ys zs)
