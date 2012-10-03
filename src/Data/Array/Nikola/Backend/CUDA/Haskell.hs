{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Array.Nikola.Backend.CUDA.Haskell
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Backend.CUDA.Haskell
    ( compile
    ) where

import Control.Applicative
import Control.Monad.State
import Data.Int
import Data.Word
import Foreign (newForeignPtr_)
import qualified Foreign.CUDA.Driver as CU
import qualified Foreign.CUDA.ForeignPtr as CU
import Foreign.Marshal (mallocArray,
                        withArray)
import Foreign.Storable (Storable)
import System.IO.Unsafe (unsafePerformIO)
-- import Text.PrettyPrint.Mainland

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.CUDA.UnboxedForeign as R

import qualified Data.Vector.Storable as V
import qualified Data.Vector.CUDA.Storable as VCS
import qualified Data.Vector.CUDA.UnboxedForeign as VCUF

import qualified Data.Array.Nikola.Backend.CUDA as N
import qualified Data.Array.Nikola.Eval as N

import Data.Array.Nikola.Backend.CUDA.Haskell.Compile
import Data.Array.Nikola.Backend.CUDA.Haskell.Ex
import qualified Data.Array.Nikola.Backend.CUDA.Nvcc as Nvcc
import Data.Array.Nikola.Backend.Flags

import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Monad (runR, REnv, emptyREnv)
import Data.Array.Nikola.Language.Optimize (optimizeHostProgram)
import Data.Array.Nikola.Language.Reify
import Data.Array.Nikola.Language.Sharing
import Data.Array.Nikola.Language.Syntax

import Data.Array.Nikola.Util.Bool

-- 'PreEx a' is the "pre-execution" state. It consists of an 'ExState', which
-- contains a reference to the CUDA module that contains our compiled kernels,
-- and a monad action of type 'Ex Val' that performs the GPu computation. The
-- phantom type is only used to drive 'Compilable' instance selection.
data PreEx a = PreEx ExState (Ex Val)

-- 'Compilable a b' says we can compile a 'PreEx a' into a 'b'. For the base
-- cases we run the monadic action in the 'PreEx' and pop the result of the
-- computaton from the environment. For inductive cases we modify the monadic
-- action in the 'PreEx' to push the argument onto the environment and perform
-- any necessary cleanup, and then induct.
--
-- Note that it is not the case that 'a' implies 'b'. For example, we can
-- compile an embedded function that takes a 'N.Array N.G sh (N.Exp a)' as an
-- argument into a function that takes a 'R.Array R.CUF rsh (N.Rep a)' a an
-- argument /or/ into a function that takes a '[N.Rep a]' as an argument. That
-- is, a DSL term that takes a Nikola global array as an argument can be
-- compiled to a Haskell term that takes a Repa CUDA array as an argument or to
-- a Haskell term that can take a Haskell list as an argument.
--
-- It is also not the case that 'b' imples 'a' for the trite reason that 'N.Rep'
-- is a type function and therefore not injective. However, morally we still
-- don't want 'b' to imply 'a' because we would like to, for example, compile
-- both pure Nikola functions that return an array and monadic Nikola
-- computations that return an array to pure Haskell functions that return an
-- array.
class Compilable a b where
    precompile :: PreEx a -> b

--
-- These are the base cases
--
instance (arep ~ N.Rep (N.Exp a),
          N.IsElem (N.Exp a),
          IsVal arep)
      => Compilable (N.Exp a) arep where
    precompile (PreEx s m) = unsafePerformIO $ evalEx (m >>= popResult) s

instance (arep ~ N.Rep a,
          IsArrayVal [arep],
          N.Load r N.DIM1 a)
      => Compilable (N.Array r N.DIM1 a)
                    [arep]               where
    precompile (PreEx s m) = unsafePerformIO $ evalEx (m >>= popResult) s

instance (arep ~ N.Rep a,
          N.IsElem a,
          N.Load r N.DIM1 a,
          Storable arep,
          IsArrayVal (V.Vector arep))
      => Compilable (N.Array  r N.DIM1 a)
                    (V.Vector arep)       where
    precompile (PreEx s m) = unsafePerformIO $ evalEx (m >>= popResult) s

instance (arep ~ N.Rep a,
          N.IsElem a,
          N.Load r N.DIM1 a,
          Storable arep,
          IsArrayVal (VCS.Vector arep))
      => Compilable (N.Array  r N.DIM1 a)
                    (VCS.Vector arep)      where
    precompile (PreEx s m) = unsafePerformIO $ evalEx (m >>= popResult) s

instance (arep ~ N.Rep a,
          rsh ~ N.Rsh sh,
          N.IsElem a,
          N.Load r N.DIM1 a,
          R.Shape rsh,
          IsArrayVal (R.Array R.CUF rsh arep),
          VCUF.UnboxForeign arep)
      => Compilable (N.Array r     sh  a   )
                    (R.Array R.CUF rsh arep) where
    precompile (PreEx s m) = unsafePerformIO $ evalEx (m >>= popResult) s

instance (arep ~ N.Rep a,
          rsh ~ N.Rsh sh,
          N.IsElem a,
          N.Load r N.DIM1 a,
          R.Shape rsh,
          IsArrayVal (R.Array R.CUF rsh arep),
          VCUF.UnboxForeign arep)
      => Compilable (N.P (N.Array r     sh  a   ))
                    (IO  (R.Array R.CUF rsh arep)) where
    precompile (PreEx s m) = evalEx (m >>= popResult) s

--
-- And here are the inductive cases
--
instance (arep ~ N.Rep (N.Exp a),
          N.IsElem (N.Exp a),
          IsVal arep,
          Compilable b c)
      => Compilable (N.Exp a -> b)
                    (arep    -> c) where
    precompile p = \x -> precompile (pushArg x p)

instance (arep ~ N.Rep a,
          IsArrayVal [arep],
          Compilable b c)
    => Compilable (N.Array N.G N.DIM1 a -> b)
                  ([arep]               -> c) where
    precompile p = \x -> precompile (pushArg x p)

instance (arep ~ N.Rep a,
          N.IsElem a,
          Storable arep,
          IsArrayVal (V.Vector arep),
          Compilable b c)
    => Compilable (N.Array  N.G N.DIM1 a -> b)
                  (V.Vector arep         -> c) where
    precompile p = \x -> precompile (pushArg x p)

instance (arep ~ N.Rep a,
          N.IsElem a,
          Storable arep,
          IsArrayVal (VCS.Vector arep),
          Compilable b c)
    => Compilable (N.Array    N.G N.DIM1 a -> b)
                  (VCS.Vector arep         -> c) where
    precompile p = \x -> precompile (pushArg x p)

instance (arep ~ N.Rep a,
          rsh ~ N.Rsh sh,
          N.IsElem a,
          R.Shape rsh,
          IsArrayVal (R.Array R.CUF rsh arep),
          VCUF.UnboxForeign arep,
          Compilable b c)
    => Compilable (N.Array N.G   sh  a    -> b)
                  (R.Array R.CUF rsh arep -> c) where
    precompile p = \x -> precompile (pushArg x p)

compile :: (Compilable a b, Reifiable a Exp) => a -> b
compile = precompile . unsafePerformIO . reifyAndCompileToEx
  where
    reifyAndCompileToEx :: Reifiable a Exp => a -> IO (PreEx a)
    reifyAndCompileToEx a = do
        (_, p)    <- runR (reify a >>= detectSharing ExpA >>= optimizeHostProgram) env
        -- putStrLn $ pretty 200 (ppr p)
        (defs, m) <- evalCEx (compileToEx p)
        -- putStrLn $ pretty 200 (ppr defs)
        mod <- Nvcc.compile defs >>= CU.loadData
        return $ PreEx (ExState mod []) m
      where
        env :: REnv
        env = emptyREnv flags

        flags :: Flags
        flags = defaultFlags { fOptimize = ljust 1 }

-- The 'IsVal' type class tells us how to push an argument onto/pop a result
-- from the environment.
class IsVal a where
    pushArg   :: a -> PreEx (b -> c) -> PreEx c
    popResult :: Val -> Ex a

#define baseTypeVal(ty,valcon)                              \
instance IsVal ty where {                                   \
; pushArg i (PreEx s m) = PreEx s (pushVal (valcon i) >> m) \
; popResult (valcon i)  = return i                          \
; popResult _           = fail "internal error: popResult" }

baseTypeVal(Bool,   BoolV)
baseTypeVal(Int8,   Int8V)
baseTypeVal(Int16,  Int16V)
baseTypeVal(Int32,  Int32V)
baseTypeVal(Int64,  Int64V)
baseTypeVal(Word8,  Word8V)
baseTypeVal(Word16, Word16V)
baseTypeVal(Word32, Word32V)
baseTypeVal(Word64, Word64V)
baseTypeVal(Float,  FloatV)
baseTypeVal(Double, DoubleV)

instance IsArrayVal [a] => IsVal [a] where
    pushArg xs (PreEx s m) = PreEx s $ do
        let sh = [length xs]
        toArrayVal xs $ \ptrs -> do
        pushVal $ ArrayV ptrs sh
        m

    popResult (ArrayV ptrs sh) = fromArrayVal ptrs sh
    popResult _                = fail "internal error: popResult [a]"

instance (Storable ty, IsArrayVal (V.Vector ty)) => IsVal (V.Vector ty) where
    pushArg xs (PreEx s m) = PreEx s $ do
        let sh = [V.length xs]
        toArrayVal xs $ \ptrs -> do
        pushVal $ ArrayV ptrs sh
        m

    popResult (ArrayV ptrs sh) = fromArrayVal ptrs sh
    popResult _                = fail "internal error: popResult (V.Vector a)"

instance (Storable ty, IsArrayVal (VCS.Vector ty)) => IsVal (VCS.Vector ty) where
    pushArg xs (PreEx s m) = PreEx s $ do
        let sh = [VCS.length xs]
        toArrayVal xs $ \ptrs -> do
        pushVal $ ArrayV ptrs sh
        m

    popResult (ArrayV ptrs sh) = fromArrayVal ptrs sh
    popResult _                = fail "internal error: popResult (V.Vector a)"

instance ( R.Shape sh
         , VCUF.UnboxForeign ty
         , IsArrayVal (R.Array R.CUF sh ty)
         ) => IsVal (R.Array R.CUF sh ty) where
    pushArg arr (PreEx s m) = PreEx s $ do
        let sh = R.listOfShape (R.extent arr)
        toArrayVal arr $ \ptrs -> do
        pushVal $ ArrayV ptrs sh
        m

    popResult (ArrayV ptrs sh) = fromArrayVal ptrs sh
    popResult _                = fail "internal error: popResult (R.Array R.CUF sh a)"

-- The 'IsArrayVal' type class tells us how to convert an 'a' to and from a
-- 'Val'.
class IsArrayVal a where
    toArrayVal   :: a -> (PtrVal -> Ex b) -> Ex b
    fromArrayVal :: PtrVal -> [Int] -> Ex a

--
-- The 'IsArrayVal' instances for lists
--

#define baseTypeListArrayVal(ty)                             \
instance IsArrayVal [ty] where {                             \
; toArrayVal xs kont = do                                    \
  { fdptr <- liftIO $ CU.mallocForeignDevPtrArray n          \
  ; liftIO $ withArray xs $ \ptr ->                          \
             CU.withForeignDevPtr fdptr $ \dptr ->           \
             CU.pokeArray n ptr dptr                         \
  ; kont $ PtrV (CU.castForeignDevPtr fdptr)                 \
  }                                                          \
  where { n :: Int; n = length xs }                          \
; fromArrayVal (PtrV fdptr) [n] = liftIO $ do                \
  { CU.withForeignDevPtr fdptr $ \dptr -> do                 \
  ; CU.peekListArray n (CU.castDevPtr dptr)                  \
  }                                                          \
; fromArrayVal _ _ = fail "internal error: fromArrayVal [a]" \
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
    toArrayVal xs kont =
        toArrayVal (map fromBool xs) kont

    fromArrayVal val n =
        map toBool <$> fromArrayVal val n

instance (IsArrayVal [a], IsArrayVal [b]) => IsArrayVal [(a, b)] where
    toArrayVal xs kont =
        toArrayVal as $ \vas -> do
        toArrayVal bs $ \vbs -> do
        kont $ TupPtrV [vas, vbs]
      where
        (as, bs) = unzip xs

    fromArrayVal (TupPtrV [vas, vbs]) sh = do
        as <- fromArrayVal vas sh
        bs <- fromArrayVal vbs sh
        return $ as `zip` bs

    fromArrayVal _ _ =
        fail "internal error: fromArrayVal [a,b]"

--
-- The 'IsArrayVal' instances for Storable Vectors
--

#define baseTypeStorableVectorArrayVal(ty)                                   \
instance IsArrayVal (V.Vector ty) where {                                    \
; toArrayVal xs kont = do                                                    \
  { fdptr <- liftIO $ CU.mallocForeignDevPtrArray n                          \
  ; liftIO $ V.unsafeWith xs $ \ptr ->                                       \
             CU.withForeignDevPtr fdptr $ \dptr ->                           \
             CU.pokeArray n ptr dptr                                         \
  ; kont $ PtrV (CU.castForeignDevPtr fdptr)                                 \
  }                                                                          \
  where { n :: Int; n = V.length xs }                                        \
; fromArrayVal (PtrV fdptr) [n] = liftIO $ do                                \
  { ptr <- liftIO $ mallocArray n                                            \
  ; CU.withForeignDevPtr fdptr $ \dptr ->                                    \
    CU.peekArray n (CU.castDevPtr dptr) ptr                                  \
  ; fptr <- newForeignPtr_ ptr                                               \
  ; return $ V.unsafeFromForeignPtr0 fptr n                                  \
  }                                                                          \
; fromArrayVal _ _ = fail "internal error: fromArrayVal (Storable.Vector a)" \
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
-- The 'IsArrayVal' instances for CUDA Storable Vectors
--

#define baseTypeCUDAStorableVectorArrayVal(ty)                          \
instance IsArrayVal (VCS.Vector ty) where {                             \
; toArrayVal v kont =                                                   \
    let { (fdptr, _) = VCS.unsafeToForeignDevPtr0 v                     \
        }                                                               \
    in kont $ PtrV (CU.castForeignDevPtr fdptr)                         \
; fromArrayVal (PtrV fdptr) [n] =                                       \
    return $ VCS.unsafeFromForeignDevPtr0                               \
                 (CU.castForeignDevPtr fdptr)                           \
                 n                                                      \
; fromArrayVal _ _ = fail "internal error: fromArrayVal (VCS.Vector a)" \
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
-- The 'IsArrayVal' instances for CUDA UnboxedForeign Vectors
--

#define baseTypeCUDAUnboxedVectorArrayVal(ty,con)                            \
instance IsArrayVal (VCUF.Vector ty) where {                             \
; toArrayVal (con v) kont =                                              \
    let { (fdptr, _) = VCS.unsafeToForeignDevPtr0 v                      \
        }                                                                \
    in kont $ PtrV (CU.castForeignDevPtr fdptr)                          \
; fromArrayVal (PtrV fdptr) [n] =                                        \
    return $ con $ VCS.unsafeFromForeignDevPtr0                          \
                 (CU.castForeignDevPtr fdptr)                            \
                 n                                                       \
; fromArrayVal _ _ = fail "internal error: fromArrayVal (VCUF.Vector a)" \
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
    toArrayVal (VCUF.V_2 _ v_a v_b) kont =
          toArrayVal v_a $ \ptr_a ->
          toArrayVal v_b $ \ptr_b ->
          kont $ TupPtrV [ptr_a, ptr_b]

    fromArrayVal (TupPtrV [ptr_a, ptr_b]) [n] = do
        v_a <- fromArrayVal ptr_a [n]
        v_b <- fromArrayVal ptr_b [n]
        return $ VCUF.V_2 n v_a v_b

    fromArrayVal _ _ =
        fail "internal error: fromArrayVal (VCUF.Vector (a, b))"

--
-- The 'IsArrayVal' instances for 'R.CUF' 'R.Array's
--

instance ( R.Shape sh
         , VCUF.UnboxForeign a
         , IsArrayVal (VCUF.Vector a)
         ) => IsArrayVal (R.Array R.CUF sh a) where
    toArrayVal arr kont =
        let v = R.toUnboxedForeign arr
        in
          toArrayVal v kont

    fromArrayVal val is =
        do let sh = R.shapeOfList is
           let sz = R.size sh
           v <- fromArrayVal val [sz]
           return $ R.fromUnboxedForeign sh v
