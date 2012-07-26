{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Data.Array.Nikola.Backend.CUDA.Haskell.Ex
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Backend.CUDA.Haskell.Ex
    ( Ex
    , ExState(..)
    , runEx
    , evalEx

    , PtrVal(..)
    , Val(..)

    , getCUDAModule
    , pushVal
    , pushVals
    , popVal
    , popVals
    ) where

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad.State
import Data.Int
import Data.Word
import qualified Foreign.CUDA.Driver as CU
import qualified Foreign.CUDA.ForeignPtr as CU
import Text.PrettyPrint.Mainland

newtype Ex a = Ex { runEx :: ExState -> IO (ExState, a) }

data ExState = ExState
  { exMod  :: !CU.Module
  , exVals :: ![Val]
  }

data PtrVal = PtrV (CU.ForeignDevicePtr Word8)
            | TupPtrV [PtrVal]

instance Pretty PtrVal where
    ppr (PtrV ptr)     = (text . show) ptr
    ppr (TupPtrV ptrs) = tuple (map ppr ptrs)

instance Show PtrVal where
    showsPrec p = shows . pprPrec p

data Val = UnitV
         | BoolV   Bool
         | Int8V   Int8
         | Int16V  Int16
         | Int32V  Int32
         | Int64V  Int64
         | Word8V  Word8
         | Word16V Word16
         | Word32V Word32
         | Word64V Word64
         | FloatV  Float
         | DoubleV Double
         | TupleV  [Val]
         | ArrayV  PtrVal [Int]
         | FunV    (Ex Val)

instance Pretty Val where
    ppr UnitV       = text "UnitV"
    ppr (BoolV   x) = text "BoolV" <+> ppr x
    ppr (Int8V   x) = text "Int8"  <+> ppr x
    ppr (Int16V  x) = text "Int16" <+> ppr x
    ppr (Int32V  x) = text "Int32" <+> ppr x
    ppr (Int64V  x) = text "Int64" <+> ppr x
    ppr (Word8V  x) = text "Word8"  <+> ppr x
    ppr (Word16V x) = text "Word16" <+> ppr x
    ppr (Word32V x) = text "Word32" <+> ppr x
    ppr (Word64V x) = text "Word64" <+> ppr x
    ppr (FloatV  x) = text "FloatV" <+> ppr x
    ppr (DoubleV x) = text "DoubleV" <+> ppr x

    ppr (TupleV xs) = text "TupleV" <+> tuple (map ppr xs)

    ppr (ArrayV ptr sh) = text "ArrayV" <+> (text . show) ptr <+> ppr sh

    ppr (FunV {}) = text "FunV"

instance Show Val where
    showsPrec p = shows . pprPrec p

evalEx :: Ex a -> ExState -> IO a
evalEx m s = snd <$> runEx m s

instance Monad Ex where
    return a = Ex $ \s -> return (s, a)

    m >>= f  = Ex $ \s -> do  (s', x) <- runEx m s
                              runEx (f x) s'

    m1 >> m2 = Ex $ \s -> do  (s', _) <- runEx m1 s
                              runEx m2 s'

    fail err = Ex $ \_ -> fail err

instance Functor Ex where
    fmap f x = x >>= return . f

instance Applicative Ex where
    pure   = return
    (<*>)  = ap

instance MonadState ExState Ex where
    get   = Ex $ \s -> return (s, s)
    put s = Ex $ \_ -> return (s, ())

instance MonadIO Ex where
    liftIO m = Ex $ \s -> do x <- m
                             return (s, x)

getCUDAModule :: Ex CU.Module
getCUDAModule = gets exMod

pushVal :: Val -> Ex ()
pushVal val =
    modify $ \s -> s { exVals = val : exVals s }

pushVals :: [Val] -> Ex ()
pushVals vals =
    modify $ \s -> s { exVals = reverse vals ++ exVals s }

popVal :: Ex ()
popVal =
    modify $ \s -> s { exVals = tail (exVals s) }

popVals :: Int -> Ex ()
popVals n =
    modify $ \s -> s { exVals = drop n (exVals s) }
