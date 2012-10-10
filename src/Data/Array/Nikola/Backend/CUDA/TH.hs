{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Array.Nikola.Backend.CUDA.TH
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Backend.CUDA.TH
    ( compileSig
    , compile
    ) where

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.Int
import Data.Word
import Foreign (mallocArray,
                withArray,
                newForeignPtr_)
import qualified Foreign.CUDA.Driver as CU
import qualified Foreign.CUDA.ForeignPtr as CU
import qualified Language.C.Syntax as C
import Language.Haskell.TH (Q, DecQ, PatQ, ExpQ)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (Quasi(..))
import qualified Language.Haskell.TH.Syntax as TH
import System.IO.Unsafe (unsafePerformIO)
-- import Text.PrettyPrint.Mainland

import qualified Data.Vector.Storable as V
import qualified Data.Vector.CUDA.Storable as VCS
import qualified Data.Vector.CUDA.Storable.Mutable as MVCS
import qualified Data.Vector.CUDA.UnboxedForeign as VCUF
import qualified Data.Vector.CUDA.UnboxedForeign.Mutable as MVCUF

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.CUDA.UnboxedForeign as R

import Data.Array.Nikola.Backend.C.Monad
import qualified Data.Array.Nikola.Backend.CUDA as N

import qualified Data.Array.Nikola.Backend.CUDA.Nvcc as Nvcc
import Data.Array.Nikola.Backend.Flags
import Data.Array.Nikola.Backend.CUDA.TH.Compile
import Data.Array.Nikola.Backend.CUDA.TH.Util

import qualified Data.Array.Nikola.Exp as E
import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Monad (runR, REnv, emptyREnv)
import Data.Array.Nikola.Language.Optimize (optimizeHostProgram)
import Data.Array.Nikola.Language.Reify
import Data.Array.Nikola.Language.Sharing
import Data.Array.Nikola.Language.Syntax

import Data.Array.Nikola.Util.Bool

-- The function 'compileToExpQ' handles generating a TH expression for the body
-- of the Nikola lambda as well as the CUDA kernels needed by the Nikola
-- computation, but we then have to coerce the Haskell arguments into the form
-- expected by this body and coerce the result of running the body back to a
-- Hakell result. The 'Compilable' class drives these coercions via instance
-- selection, and the coercions themselves are built up in the 'NQ' monad. Note
-- that we need to gensym binders sometimes that are bound in the Haskell lambda
-- but show up in the right-hand side of a declaration, so the 'NQ' monad must
-- be an instance of 'Quasi'.

newtype NQ a = NQ { runNQ :: NQEnv -> Q (NQEnv, a) }

execNQ :: [CudaKernel] -> [C.Definition] -> [(Var, Type)] -> ExpQ -> NQ () -> Q NQEnv
execNQ kernels cdefs vtaus qbody m =
    fst <$> runNQ m (defaultNQEnv kernels cdefs vtaus qbody)

data NQEnv = NQEnv
    { nqKernels     :: [CudaKernel]   -- ^ The list of CUDA kernels the Nikola
                                      -- program calls.
    , nqCDefs       :: [C.Definition] -- ^ The C definitions that comprise the
                                      -- CUDA program containing the CUDA
                                      -- kernels needed by the Nikola program.
    , nqLamVars     :: [(Var, Type)]  -- ^ The variables bound by the top-level
                                      -- Nikola lambda.
    , nqLamPats     :: [TH.Pat]       -- ^ The patterns that the compiled
                                      -- program uses to bind the variables in
                                      -- the top-level Nikola lambda.
    , nqDecs        :: [TH.Dec]       -- ^ Declarations needed by the compiled
                                      -- Nikola program. This consists of a
                                      -- binding for the compiled CUDA module,
                                      -- of type 'CU.Module', as well as
                                      -- bindings for all the CUDA kernels, of
                                      -- type 'CU.Fun', used by the Nikola
                                      -- program.
    , nqBody        :: TH.ExpQ        -- ^ The body of the compiled Nikola
                                      -- program.
    , nqMorallyPure :: Bool           -- ^ The function is morally pure, so add
                                      -- an 'unsafePerformIO' if the computation
                                      -- is monadic.
    }

defaultNQEnv :: [CudaKernel] -> [C.Definition] -> [(Var, Type)] -> ExpQ -> NQEnv
defaultNQEnv kernels cdefs vtaus qbody = NQEnv
    { nqKernels     = kernels
    , nqCDefs       = cdefs
    , nqLamVars     = vtaus
    , nqLamPats     = []
    , nqDecs        = []
    , nqBody        = qbody
    , nqMorallyPure = False
    }

instance Monad NQ where
    return a = NQ $ \s -> return (s, a)

    m >>= f  = NQ $ \s -> do  (s', x) <- runNQ m s
                              runNQ (f x) s'

    m1 >> m2 = NQ $ \s -> do  (s', _) <- runNQ m1 s
                              runNQ m2 s'

    fail err = NQ $ \_ -> fail err

instance Functor NQ where
    fmap f x = x >>= return . f

instance Applicative NQ where
    pure   = return
    (<*>)  = ap

instance MonadState NQEnv NQ where
    get   = NQ $ \s -> return (s, s)
    put s = NQ $ \_ -> return (s, ())

instance MonadIO NQ where
    liftIO m = NQ $ \s -> do x <- liftIO m
                             return (s, x)

liftQ :: Q a -> NQ a
liftQ m = NQ $ \s -> do x <- m
                        return (s, x)

instance Quasi NQ where
    qNewName = liftQ . qNewName

    qReport flag msg = liftQ $ qReport flag msg

    qRecover _ _ = fail "qRecover: punting"

    qLookupName ns s = liftQ $ qLookupName ns s

    qReify = liftQ . qReify

    qReifyInstances n tys = liftQ $ qReifyInstances n tys

    qLocation = liftQ qLocation

    qRunIO = liftIO

    qAddDependentFile = liftQ . qAddDependentFile

takeLamVar :: NQ (Var, Type)
takeLamVar = do
    vtaus <- gets nqLamVars
    vtau  <- case vtaus of
               vtau:_ -> return vtau
               _      -> fail "internal error in takeLamVar: no lambdas left!"
    modify $ \s -> s { nqLamVars = tail (nqLamVars s) }
    return vtau

appendLamPats :: [TH.Pat] -> NQ ()
appendLamPats ps =
    modify $ \s -> s { nqLamPats = nqLamPats s ++ ps }

appendDec :: [TH.Dec] -> NQ ()
appendDec ps =
    modify $ \s -> s { nqDecs = nqDecs s ++ ps }

modifyBody :: (ExpQ -> ExpQ) -> NQ ()
modifyBody f = do
    qbody <- gets nqBody
    modify $ \s -> s { nqBody = f qbody }

addResultCoercion :: (ExpQ -> ExpQ) -> PreExpQ a -> PreExpQ b
addResultCoercion f (PreExpQ m) = PreExpQ $ do
    m
    qbody <- gets nqBody
    modify $ \s -> s { nqBody = f qbody }

addArgBinder :: NQ () -> PreExpQ (a -> b) -> PreExpQ b
addArgBinder m' (PreExpQ m) = PreExpQ (m >> m')

setMorallyPure :: Bool -> PreExpQ a -> PreExpQ a
setMorallyPure isPure (PreExpQ m) = PreExpQ $ do
    m
    modify $ \s -> s { nqMorallyPure = isPure }

newtype PreExpQ a = PreExpQ { unPreExpQ :: NQ () }

castPreExpQ :: PreExpQ a -> PreExpQ b
castPreExpQ (PreExpQ qe) = PreExpQ qe

-- 'Compilable' relates the type of a Nikola DSL expression to the type of the
-- compiled version of the DSL expression. It really is a relation! The type
-- index serves only to drive instance selection; we recursive over the index
-- @a@, which is the type of a Nikola DSL term, and transform it into a @b@,
-- which is the type of the compiled version of the Nikola DSL term, building up
-- the 'PreExpQ' along the way.
class Compilable a b where
    precompile :: PreExpQ a -> PreExpQ b

-- 'Rsh' is a type function from Nikola shapes to Repa shapes. This allows
-- 'Compilable' instances that involve arrays to be polymorphic in array shape.
class ToRsh a where
    type Rsh a :: *

    toRshPatQ :: a -> [PatQ] -> PatQ
    toRshExpQ :: a -> [ExpQ] -> ExpQ

instance ToRsh N.Z where
    type Rsh N.Z = R.Z

    toRshPatQ _ _ = [p|R.Z|]
    toRshExpQ _ _ = [|R.Z|]

instance ToRsh sh => ToRsh (sh N.:. E.Exp N.CUDA E.Ix) where
    type Rsh (sh N.:. E.Exp N.CUDA E.Ix) = Rsh sh R.:. Int

    toRshPatQ _ qps =
        TH.infixP (toRshPatQ (undefined :: sh) (init qps))
                  (TH.mkName "R.:.")
                  (last qps)

    toRshExpQ _ qes =
        [|$(toRshExpQ (undefined :: sh) (init qes)) R.:. $(last qes)|]

--
-- These are the base cases
--
instance (arep ~ N.Rep (N.Exp a),
          N.IsElem (N.Exp a),
          IsVal arep)
      => Compilable (N.Exp a) arep where
    precompile =
        setMorallyPure True .
        addResultCoercion (coerceResult (undefined :: arep))

instance (arep ~ N.Rep a,
          N.IsElem a,
          IsArrayVal [arep])
      => Compilable (N.Array r N.DIM1 a)
                    [arep]               where
    precompile =
        setMorallyPure True .
        addResultCoercion (coerceResult (undefined :: [arep]))

instance (arep ~ N.Rep a,
          N.IsElem a,
          IsArrayVal (V.Vector arep))
      => Compilable (N.Array r N.DIM1 a)
                    (V.Vector arep     ) where
    precompile =
        setMorallyPure True .
        addResultCoercion (coerceResult (undefined :: V.Vector arep))

instance (arep ~ N.Rep (N.Exp a),
          N.IsElem (N.Exp a),
          IsArrayVal (VCS.Vector arep))
      => Compilable (N.Array r N.DIM1 (N.Exp a))
                    (VCS.Vector arep           ) where
    precompile =
        setMorallyPure True .
        addResultCoercion (coerceResult (undefined :: VCS.Vector arep))

instance (arep ~ N.Rep a,
          rsh ~ Rsh sh,
          ToRsh sh,
          N.Shape sh,
          N.IsElem a,
          IsVal (R.Array R.CUF rsh arep))
      => Compilable (N.Array r     sh  a)
                    (R.Array R.CUF rsh arep) where
    precompile =
        setMorallyPure True .
        addResultCoercion (coerceResult (undefined :: R.Array R.CUF rsh arep))

instance Compilable (N.P ())
                    (IO  ()) where
    precompile =
        addResultCoercion id

instance (arep ~ N.Rep a,
          rsh ~ Rsh sh,
          ToRsh sh,
          N.Shape sh,
          N.IsElem a,
          IsVal (R.Array R.CUF rsh arep))
      => Compilable (N.P (N.Array r     sh  a))
                    (IO  (R.Array R.CUF rsh arep)) where
    precompile =
        addResultCoercion $ coerceResult (undefined :: R.Array R.CUF rsh arep)

--
-- And here are the inductive cases
--
instance (arep ~ N.Rep (N.Exp a),
          N.IsElem (N.Exp a),
          IsVal arep,
          Compilable b c)
    => Compilable (N.Exp a -> b)
                  (arep    -> c) where
    precompile pq =
        castPreExpQ (precompile p' :: PreExpQ c)
      where
        p' :: PreExpQ b
        p' = addArgBinder (bindArgs (undefined :: arep)) pq

instance (arep ~ N.Rep a,
          N.IsElem a,
          IsArrayVal [arep],
          Compilable b c)
    => Compilable (N.Array N.G N.DIM1 a -> b)
                  ([arep]               -> c) where
    precompile pq =
        castPreExpQ (precompile p' :: PreExpQ c)
      where
        p' :: PreExpQ b
        p' = addArgBinder (bindArgs (undefined :: [arep])) pq

instance (arep ~ N.Rep a,
          N.IsElem a,
          IsArrayVal (V.Vector arep),
          Compilable b c)
    => Compilable (N.Array N.G N.DIM1 a -> b)
                  (V.Vector arep        -> c) where
    precompile pq =
        castPreExpQ (precompile p' :: PreExpQ c)
      where
        p' :: PreExpQ b
        p' = addArgBinder (bindArgs (undefined :: V.Vector arep)) pq

instance (arep ~ N.Rep a,
          N.IsElem a,
          IsArrayVal (VCS.Vector arep),
          Compilable b c)
    => Compilable (N.Array N.G N.DIM1 a -> b)
                  (VCS.Vector arep      -> c) where
    precompile pq =
        castPreExpQ (precompile p' :: PreExpQ c)
      where
        p' :: PreExpQ b
        p' = addArgBinder (bindArgs (undefined :: VCS.Vector arep)) pq

instance (rsh ~ Rsh sh,
          arep ~ N.Rep a,
          ToRsh sh,
          N.Shape sh,
          R.Shape (Rsh sh),
          N.IsElem a,
          IsVal (R.Array R.CUF rsh arep),
          Compilable b c)
    => Compilable (N.Array N.G   sh  a    -> b)
                  (R.Array R.CUF rsh arep -> c) where
    precompile pq =
        castPreExpQ (precompile p' :: PreExpQ c)
      where
        p' :: PreExpQ b
        p' = addArgBinder (bindArgs (undefined :: R.Array R.CUF rsh arep)) pq

instance (rsh ~ Rsh sh,
          arep ~ N.Rep a,
          ToRsh sh,
          N.Shape sh,
          R.Shape (Rsh sh),
          N.IsElem a,
          IsVal (R.MArray R.CUF rsh arep),
          Compilable b c)
    => Compilable (N.MArray N.G   sh  a    -> b)
                  (R.MArray R.CUF rsh arep -> c) where
    precompile pq =
        castPreExpQ (precompile p' :: PreExpQ c)
      where
        p' :: PreExpQ b
        p' = addArgBinder (bindArgs (undefined :: R.MArray R.CUF rsh arep)) pq

-- | If we can reify a value of type @a@ as a Nikola program, and if we can
-- compile a value of type @a@ into a value of type @b@, then 'compileSig' will
-- reify, optimize, and compile it. Note that the second argument of type @b@
-- serves only to specify the type of the final compiled value---the value
-- itself is never evaluated, so it is perfectly acceptable (and expected!) to
-- pass 'undefined' as the second argument (with a proper type signature of
-- course!).
compileSig :: forall a b . (Compilable a b, Reifiable a Exp)
           => a
           -> b
           -> ExpQ
compileSig a _ = do
    (_, p)  <- liftIO $ flip runR env $
               reify a >>= detectSharing ExpA >>= optimizeHostProgram
    (kernels, cdefs, (vtaus, qbody, isMonadic)) <- liftIO $ evalCEx (compileToExpQ p)
    let pexpq :: PreExpQ a
        pexpq = PreExpQ (return ())
        pexpq' :: PreExpQ b
        pexpq' = precompile pexpq
    execNQ kernels cdefs vtaus qbody (unPreExpQ pexpq') >>= finalizeExpQ isMonadic
  where
    env :: REnv
    env = emptyREnv flags

    flags :: Flags
    flags = defaultFlags { fOptimize = ljust 1 }

finalizeExpQ :: Bool -> NQEnv -> ExpQ
finalizeExpQ isMonadic env = do
    kernelDecQs <- kernelDecQs (nqKernels env) (nqCDefs env)
    TH.lamE (map return (nqLamPats env)) $
      TH.letE (kernelDecQs ++ map return (nqDecs env)) $
      if isMonadic && nqMorallyPure env
      then [|unsafePerformIO $(nqBody env)|]
      else [|$(nqBody env)|]
  where
    kernelDecQs :: [CudaKernel] -> [C.Definition] -> Q [DecQ]
    kernelDecQs [] _ =
        return []

    kernelDecQs kerns defs = do
        -- liftIO $ putStrLn $ pretty 200 $ ppr defs
        bs      <- liftIO $ Nvcc.compile defs
        modName <- TH.qNewName "module"
        return $ modDecQ modName bs :
                 map (workBlockCounterDecQ modName) (concatMap cukernWorkBlocks kerns) ++
                 map (kernDecQ modName) kerns
      where
        -- Generate the binding for the 'CU.Module' containing the CUDA kernels.
        modDecQ :: TH.Name -> B.ByteString -> DecQ
        modDecQ modName bs = do
            let qbs    = TH.litE (TH.stringL (B.unpack bs))
            let qbody  = [|unsafePerformIO $ CU.loadData $ B.pack $qbs|]
            TH.valD (TH.varP modName) (TH.normalB qbody) []

        -- Generate bindings for a work block counter
        workBlockCounterDecQ :: TH.Name -> CudaWorkBlock -> DecQ
        workBlockCounterDecQ modName wb = do
            let blockCounterName = TH.mkName (cuworkBlockCounter wb)
            let qbody            = [|unsafePerformIO $ CU.getPtr $(TH.varE modName) $(TH.stringE (cuworkBlockCounter wb))|]
            TH.valD (TH.tupP [TH.varP blockCounterName, TH.wildP]) (TH.normalB qbody) []

        -- Generate the binding for the CUDA kernel @kern@.
        kernDecQ :: TH.Name -> CudaKernel -> DecQ
        kernDecQ modName kern = do
            let kernName = TH.mkName (cukernName kern)
            let qbody    = [|unsafePerformIO $ CU.getFun $(TH.varE modName) $(TH.stringE (cukernName kern))|]
            TH.valD (TH.varP kernName) (TH.normalB qbody) []

-- | If you only want the default compiled version of a Nikola program, use
-- 'compile'. The default compilation scheme compiles Nikola expressions to
-- their "natural" Haskell representation, i.e., 'Exp t Double' is compiled to
-- 'Double', and compiles Nikola arrays to Repa arrays with the type tag
-- 'R.CUF'.
compile :: forall a . (Compilable a (Compiled a), Reifiable a Exp) => a -> ExpQ
compile a = compileSig a (undefined :: Compiled a)

-- The 'Compiled' type function specifies the default compilation scheme for
-- 'compile'.
type family Compiled a :: *
type instance Compiled (N.P ())                  = IO ()
type instance Compiled (N.Exp a)                 = N.Rep (N.Exp a)
type instance Compiled (N.Array r sh a)          = R.Array R.CUF (Rsh sh) (N.Rep a)
type instance Compiled (N.P (N.Array N.G sh a))  = IO (R.Array R.CUF (Rsh sh) (N.Rep a))

type instance Compiled (N.Exp a           -> b)  = N.Rep (N.Exp a) -> Compiled b
type instance Compiled (N.Array  N.G sh a -> b)  = R.Array  R.CUF (Rsh sh) (N.Rep a) -> Compiled b
type instance Compiled (N.MArray N.G sh a -> b)  = R.MArray R.CUF (Rsh sh) (N.Rep a) -> Compiled b

-- The 'IsVal' type class tells us how to bind arguments in the form expected by
-- the TH expression representation of a Nikola program and how to coerce the
-- results returned by the Nikola program into the desired form.
class IsVal a where
    bindArgs     :: a -> NQ ()
    coerceResult :: a -> ExpQ -> ExpQ

#define baseTypeVal(ty)             \
instance IsVal ty where {           \
; bindArgs _ = do                   \
  { v <- fst <$> takeLamVar         \
  ; p <- liftQ $ TH.varP (mkName v) \
  ; appendLamPats [p]               \
  }                                 \
; coerceResult _ = id               \
}

baseTypeVal(Bool)
baseTypeVal(Int8)
baseTypeVal(Int16)
baseTypeVal(Int32)
baseTypeVal(Int64)
baseTypeVal(Word8)
baseTypeVal(Word16)
baseTypeVal(Word32)
baseTypeVal(Word64)
baseTypeVal(Float)
baseTypeVal(Double)

--
-- Val instance for CUDA UnboxedForeign arrays
--

instance IsArrayVal (VCUF.Vector a) => IsVal (R.Array R.CUF rsh a) where
    bindArgs _ = do
        arr  <- fst <$> takeLamVar
        rarr <- qNewName "repa_arr"
        p    <- liftQ $ TH.varP rarr
        appendLamPats [p]

        modifyBody $ \qm ->
            [|do { let { v = R.toUnboxedForeign $(TH.varE rarr) }
                 ; $(coerceArrayArg (undefined :: VCUF.Vector a)) v $ \ptrs ->
                   $(lamsE [arr] qm) (NArray ptrs (R.extent $(TH.varE rarr)))
                 }
             |]

    coerceResult _ qm =
        [|$qm >>= \(NArray fdptrs sh) ->
           do { let sz = R.size sh
              ; v <- $(coerceArrayResult (undefined :: VCUF.Vector a)) fdptrs (R.Z R.:. sz)
              ; return $ R.fromUnboxedForeign sh v
              }
         |]

--
-- Val instance for mutable CUDA UnboxedForeign arrays
--

instance IsArrayVal (MVCUF.IOVector a) => IsVal (R.MArray R.CUF sh a) where
    bindArgs _ = do
        arr  <- fst <$> takeLamVar
        rarr <- qNewName "repa_marr"
        p    <- liftQ $ TH.varP rarr
        appendLamPats [p]

        modifyBody $ \qm ->
            [|do { let { R.MCFUnboxed sh v = $(TH.varE rarr) }
                 ; $(coerceArrayArg (undefined :: MVCUF.IOVector a)) v $ \ptrs ->
                   $(lamsE [arr] qm) (NArray ptrs sh)
                 }
             |]

    coerceResult _ qm =
        [|$qm >>= \(NArray fdptrs sh) ->
           do { let sz = R.size sh
              ; v <- $(coerceArrayResult (undefined :: MVCUF.IOVector a)) fdptrs (R.Z R.:. sz)
              ; return $ R.MCFUnboxed sh v
              }
         |]

--
-- Val instances for array-like things
--

-- The 'IsArrayVal' type class tells us how to bind an 'a' and how to convert an
-- 'a' to and from a Repa CF array.
class IsArrayVal a where
    coerceArrayArg    :: a -> ExpQ
    coerceArrayResult :: a -> ExpQ

instance IsArrayVal [a] => IsVal [a] where
    bindArgs _ = do
        arr <- fst <$> takeLamVar
        xs  <- qNewName "xs"
        p   <- liftQ $ TH.varP xs
        appendLamPats [p]

        modifyBody $ \qm ->
            [|$(coerceArrayArg (undefined :: [a])) $(TH.varE xs) $ \ptrs ->
              $(lamsE [arr] qm) (NArray ptrs (R.ix1 (length $(TH.varE xs))))
             |]

    coerceResult _ qm =
        [|$qm >>= \(NArray fdptrs sh) -> $(coerceArrayResult (undefined :: [a])) fdptrs sh|]

instance IsArrayVal (V.Vector a) => IsVal (V.Vector a) where
    bindArgs _ = do
        arr <- fst <$> takeLamVar
        xs  <- qNewName "xs"
        p   <- liftQ $ TH.varP xs
        appendLamPats [p]

        modifyBody $ \qm ->
            [|$(coerceArrayArg (undefined :: V.Vector a)) $(TH.varE xs) $ \ptrs ->
              $(lamsE [arr] qm) (NArray ptrs (R.ix1 (V.length $(TH.varE xs))))
             |]

    coerceResult _ qm =
        [|$qm >>= \(NArray fdptrs sh) -> $(coerceArrayResult (undefined :: V.Vector a)) fdptrs sh|]

instance IsArrayVal (VCS.Vector a) => IsVal (VCS.Vector a) where
    bindArgs _ = do
        arr <- fst <$> takeLamVar
        xs  <- qNewName "xs"
        p   <- liftQ $ TH.varP xs
        appendLamPats [p]

        modifyBody $ \qm ->
            [|$(coerceArrayArg (undefined :: VCS.Vector a)) $(TH.varE xs) $ \ptrs ->
              $(lamsE [arr] qm) (NArray ptrs (R.ix1 (VCS.length $(TH.varE xs))))
             |]

    coerceResult _ qm =
        [|$qm >>= \(NArray fdptrs sh) -> $(coerceArrayResult (undefined :: VCS.Vector a)) fdptrs sh|]

--
-- 'IsArrayVal' instances for lists
--

#define baseTypeListArrayVal(ty)                  \
instance IsArrayVal [ty] where {                  \
; coerceArrayArg _ =                              \
  [|\xs kont ->                                   \
    do { let { n = length xs }                    \
       ; fdptr <- CU.mallocForeignDevPtrArray n   \
       ; withArray xs $ \ptr ->                   \
            CU.withForeignDevPtr fdptr $ \dptr -> \
            CU.pokeArray n ptr dptr               \
       ; kont fdptr                               \
       }                                          \
   |]                                             \
; coerceArrayResult _ =                           \
  [|\fdptr (R.Z R.:. n) ->                        \
    do { CU.withForeignDevPtr fdptr $ \dptr ->    \
           CU.peekListArray n dptr                \
       }                                          \
   |]                                             \
}

baseTypeListArrayVal(Int8)
baseTypeListArrayVal(Int16)
baseTypeListArrayVal(Int32)
baseTypeListArrayVal(Int64)
baseTypeListArrayVal(Word8)
baseTypeListArrayVal(Word16)
baseTypeListArrayVal(Word32)
baseTypeListArrayVal(Word64)
baseTypeListArrayVal(Float)
baseTypeListArrayVal(Double)

instance IsArrayVal [Bool] where
    coerceArrayArg _ =
        [|\xs kont ->
          do let n = length xs
             fdptr <- CU.mallocForeignDevPtrArray n
             withArray (map fromBool xs) $ \ptr ->
                CU.withForeignDevPtr fdptr $ \dptr ->
                CU.pokeArray n ptr dptr
             kont fdptr
         |]

    coerceArrayResult _ =
        [|\fdptr (R.Z R.:. n) ->
          do xs <- CU.withForeignDevPtr fdptr $ \dptr ->
                   CU.peekListArray n dptr
             return $ map toBool  xs
         |]

instance (IsArrayVal [a], IsArrayVal [b]) => IsArrayVal [(a, b)] where
    coerceArrayArg _ =
        [|\xs kont ->
          do let (as, bs) = unzip xs
             $(coerceArrayArg (undefined :: [a])) as $ \arra -> do
             $(coerceArrayArg (undefined :: [b])) bs $ \arrb -> do
             kont (arra, arrb)
         |]

    coerceArrayResult _ =
        [|\(arra, arrb) sh -> do
           as <- $(coerceArrayResult (undefined :: [a])) arra sh
           bs <- $(coerceArrayResult (undefined :: [b])) arrb sh
           return (as `zip` bs)
         |]

--
-- 'IsArrayVal' instances for Storable 'Vector's
--

#define baseTypeStorableVectorArrayVal(ty)        \
instance IsArrayVal (V.Vector ty) where {         \
; coerceArrayArg _ =                              \
  [|\xs kont ->                                   \
    do { let { n = V.length xs }                  \
       ; fdptr <- CU.mallocForeignDevPtrArray n   \
       ; V.unsafeWith xs $ \ptr ->                \
            CU.withForeignDevPtr fdptr $ \dptr -> \
            CU.pokeArray n ptr dptr               \
       ; kont fdptr                               \
       }                                          \
   |]                                             \
; coerceArrayResult _ =                           \
  [|\fdptr (R.Z R.:. n) ->                        \
    do { ptr <- liftIO $ mallocArray n            \
       ; CU.withForeignDevPtr fdptr $ \dptr ->    \
           CU.peekArray n dptr ptr                \
       ; fptr <- newForeignPtr_ ptr               \
       ; return $ V.unsafeFromForeignPtr0 fptr n  \
       }                                          \
   |]                                             \
}

baseTypeStorableVectorArrayVal(Int8)
baseTypeStorableVectorArrayVal(Int16)
baseTypeStorableVectorArrayVal(Int32)
baseTypeStorableVectorArrayVal(Int64)
baseTypeStorableVectorArrayVal(Word8)
baseTypeStorableVectorArrayVal(Word16)
baseTypeStorableVectorArrayVal(Word32)
baseTypeStorableVectorArrayVal(Word64)
baseTypeStorableVectorArrayVal(Float)
baseTypeStorableVectorArrayVal(Double)

--
-- 'IsArrayVal' instances for CUDA Storable 'Vector's
--

#define baseTypeCUDAStorableVectorArrayVal(ty)             \
instance IsArrayVal (VCS.Vector ty) where {                \
; coerceArrayArg _ =                                       \
  [|\v kont ->                                             \
    do { let { (fdptr, _) = VCS.unsafeToForeignDevPtr0 v } \
       ; kont fdptr                                        \
       }                                                   \
   |]                                                      \
; coerceArrayResult _ =                                    \
  [|\fdptr (R.Z R.:. n) ->                                 \
        return $ VCS.unsafeFromForeignDevPtr0 fdptr n      \
   |]                                                      \
}

baseTypeCUDAStorableVectorArrayVal(Int8)
baseTypeCUDAStorableVectorArrayVal(Int16)
baseTypeCUDAStorableVectorArrayVal(Int32)
baseTypeCUDAStorableVectorArrayVal(Int64)
baseTypeCUDAStorableVectorArrayVal(Word8)
baseTypeCUDAStorableVectorArrayVal(Word16)
baseTypeCUDAStorableVectorArrayVal(Word32)
baseTypeCUDAStorableVectorArrayVal(Word64)
baseTypeCUDAStorableVectorArrayVal(Float)
baseTypeCUDAStorableVectorArrayVal(Double)

--
-- 'IsArrayVal' instances for CUDA UnboxedForeign Vectors
--

#define baseTypeCUDAUnboxedVectorArrayVal(ty,con)           \
instance IsArrayVal (VCUF.Vector ty) where {                \
; coerceArrayArg _ =                                        \
  [|\(con v) kont ->                                        \
    do { let { (fdptr, _) = VCS.unsafeToForeignDevPtr0 v }  \
       ; kont fdptr                                         \
       }                                                    \
   |]                                                       \
; coerceArrayResult _ =                                     \
  [|\fdptr (R.Z R.:. n) ->                                  \
        return $ con $ VCS.unsafeFromForeignDevPtr0 fdptr n \
   |]                                                       \
}

baseTypeCUDAUnboxedVectorArrayVal(Int8,   VCUF.V_Int8)
baseTypeCUDAUnboxedVectorArrayVal(Int16,  VCUF.V_Int16)
baseTypeCUDAUnboxedVectorArrayVal(Int32,  VCUF.V_Int32)
baseTypeCUDAUnboxedVectorArrayVal(Int64,  VCUF.V_Int64)
baseTypeCUDAUnboxedVectorArrayVal(Word8,  VCUF.V_Word8)
baseTypeCUDAUnboxedVectorArrayVal(Word16, VCUF.V_Word16)
baseTypeCUDAUnboxedVectorArrayVal(Word32, VCUF.V_Word32)
baseTypeCUDAUnboxedVectorArrayVal(Word64, VCUF.V_Word64)
baseTypeCUDAUnboxedVectorArrayVal(Float,  VCUF.V_Float)
baseTypeCUDAUnboxedVectorArrayVal(Double, VCUF.V_Double)

instance ( IsArrayVal (VCUF.Vector a)
         , IsArrayVal (VCUF.Vector b)
         ) => IsArrayVal (VCUF.Vector (a, b)) where
    coerceArrayArg _ =
        [|\(VCUF.V_2 _ v_a v_b) kont ->
          $(coerceArrayArg (undefined :: VCUF.Vector a)) v_a $ \ptr_a ->
          $(coerceArrayArg (undefined :: VCUF.Vector b)) v_b $ \ptr_b ->
          kont (ptr_a, ptr_b)
         |]

    coerceArrayResult _ =
        [|\(ptr_a, ptr_b) sh@(R.Z R.:. n) ->
          do { v_a <- $(coerceArrayResult (undefined :: VCUF.Vector a)) ptr_a sh
             ; v_b <- $(coerceArrayResult (undefined :: VCUF.Vector b)) ptr_b sh
             ; return $ VCUF.V_2 n v_a v_b
             }
         |]

--
-- 'IsArrayVal' instances for mutable CUDA UnboxedForeign Vectors
--

#define baseTypeCUDAUnboxedMVectorArrayVal(ty,con)           \
instance IsArrayVal (MVCUF.IOVector ty) where {              \
; coerceArrayArg _ =                                         \
  [|\(con v) kont ->                                         \
    do { let { (fdptr, _) = MVCS.unsafeToForeignDevPtr0 v }  \
       ; kont fdptr                                          \
       }                                                     \
   |]                                                        \
; coerceArrayResult _ =                                      \
  [|\fdptr (R.Z R.:. n) ->                                   \
        return $ con $ MVCS.unsafeFromForeignDevPtr0 fdptr n \
   |]                                                        \
}

baseTypeCUDAUnboxedMVectorArrayVal(Int8,   MVCUF.MV_Int8)
baseTypeCUDAUnboxedMVectorArrayVal(Int16,  MVCUF.MV_Int16)
baseTypeCUDAUnboxedMVectorArrayVal(Int32,  MVCUF.MV_Int32)
baseTypeCUDAUnboxedMVectorArrayVal(Int64,  MVCUF.MV_Int64)
baseTypeCUDAUnboxedMVectorArrayVal(Word8,  MVCUF.MV_Word8)
baseTypeCUDAUnboxedMVectorArrayVal(Word16, MVCUF.MV_Word16)
baseTypeCUDAUnboxedMVectorArrayVal(Word32, MVCUF.MV_Word32)
baseTypeCUDAUnboxedMVectorArrayVal(Word64, MVCUF.MV_Word64)
baseTypeCUDAUnboxedMVectorArrayVal(Float,  MVCUF.MV_Float)
baseTypeCUDAUnboxedMVectorArrayVal(Double, MVCUF.MV_Double)

instance ( IsArrayVal (MVCUF.IOVector a)
         , IsArrayVal (MVCUF.IOVector b)
         ) => IsArrayVal (MVCUF.IOVector (a, b)) where
    coerceArrayArg _ =
        [|\(MVCUF.MV_2 _ v_a v_b) kont ->
          $(coerceArrayArg (undefined :: MVCUF.IOVector a)) v_a $ \ptr_a ->
          $(coerceArrayArg (undefined :: MVCUF.IOVector b)) v_b $ \ptr_b ->
          kont (ptr_a, ptr_b)
         |]

    coerceArrayResult _ =
        [|\(ptr_a, ptr_b) sh@(R.Z R.:. n) ->
          do { v_a <- $(coerceArrayResult (undefined :: MVCUF.IOVector a)) ptr_a sh
             ; v_b <- $(coerceArrayResult (undefined :: MVCUF.IOVector b)) ptr_b sh
             ; return $ MVCUF.MV_2 n v_a v_b
             }
         |]
