{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Data.Array.Nikola.Backend.CUDA.TH.Util
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Backend.CUDA.TH.Util where

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad.State
import Foreign.Storable (Storable)
import Data.Int
import Data.Word
import qualified Foreign.CUDA.Driver as CU
import qualified Foreign.CUDA.ForeignPtr as CU
import Language.Haskell.TH (Q, DecQ, PatQ, ExpQ, StmtQ)
import qualified Language.Haskell.TH as TH

import Data.Array.Nikola.Language.Syntax

import qualified Data.Array.Repa as R

-- Odd that this instance isn't defined anywhere...
instance MonadIO Q where
    liftIO = TH.runIO

mkName :: Var -> TH.Name
mkName = TH.mkName . unVar

varP :: Var -> PatQ
varP v = TH.varP (mkName v)

lamsE :: [Var] -> ExpQ -> ExpQ
lamsE vs qe = TH.lamE (map varP vs) qe

varE :: Var -> ExpQ
varE v = TH.varE (mkName v)

valD :: Var -> ExpQ -> DecQ
valD v qe = TH.valD (varP v) (TH.normalB qe) []

letS :: Var -> ExpQ -> StmtQ
letS v qe = TH.letS [valD v qe]

bindS :: Var -> ExpQ -> StmtQ
bindS v qe = TH.bindS (varP v) qe

tupM :: [ExpQ] -> ExpQ
tupM []       = [|()|]
tupM [me]     = [|$me|]
tupM (me:mes) = app [|$tupCon <$> $me|] mes
  where
    tupCon :: ExpQ
    tupCon = TH.conE (TH.tupleDataName (1+(length mes)))

    app :: ExpQ -> [ExpQ] -> ExpQ
    app f []       = f
    app f (me:mes) = app [|$f <*> $me|] mes

firstTup :: Int -> ExpQ -> ExpQ
firstTup n qe = do
    let x  = TH.mkName "x"
    let qp = TH.tupP (TH.varP x : replicate (n - 1) TH.wildP)
    TH.caseE qe [TH.match qp (TH.normalB (TH.varE x)) []]

-- | @ptrs@ is a singleton or (potentially nested) tuple of 'ForeignDevicePtr's,
-- and @sh@ is a shape.
data NikolaArray ptrs sh = NArray !ptrs !sh

class ToFunParams a where
    toFunParams :: a -> ([CU.FunParam] -> IO b) -> IO b

#define baseTypeToFunParams(ty)         \
instance ToFunParams ty where {         \
; {-# INLINE toFunParams #-}            \
; toFunParams x kont = kont [CU.VArg x] \
}

baseTypeToFunParams(Int8)
baseTypeToFunParams(Int16)
baseTypeToFunParams(Int32)
baseTypeToFunParams(Int64)
baseTypeToFunParams(Word8)
baseTypeToFunParams(Word16)
baseTypeToFunParams(Word32)
baseTypeToFunParams(Word64)
baseTypeToFunParams(Float)
baseTypeToFunParams(Double)

instance ToFunParams Int where
    {-# INLINE toFunParams #-}
    toFunParams i = toFunParams (fromIntegral i :: Int32)

instance ToFunParams Bool where
    {-# INLINE toFunParams #-}
    toFunParams False = toFunParams (0 :: Word8)
    toFunParams True  = toFunParams (1 :: Word8)

instance (Storable a) => ToFunParams (CU.ForeignDevicePtr a) where
    {-# INLINE toFunParams #-}
    toFunParams fdptr kont =
        CU.withForeignDevPtr fdptr $ \dptr ->
        kont [CU.VArg dptr]

instance ToFunParams R.Z where
    {-# INLINE toFunParams #-}
    toFunParams R.Z kont = kont []

instance ToFunParams sh => ToFunParams (sh R.:. Int) where
    {-# INLINE toFunParams #-}
    toFunParams (sh R.:. i) kont =
        toFunParams sh $ \fparams_sh -> do
        toFunParams i  $ \fparams_i  -> do
        kont (fparams_sh ++ fparams_i)

instance (ToFunParams ptrs, ToFunParams sh) => ToFunParams (NikolaArray ptrs sh) where
    {-# INLINE toFunParams #-}
    toFunParams (NArray ptrs sh) kont =
        toFunParams ptrs $ \fparams_ptrs -> do
        toFunParams sh   $ \fparams_sh -> do
        kont (fparams_ptrs ++ fparams_sh)

instance (ToFunParams a, ToFunParams b) => ToFunParams (a, b) where
    {-# INLINE toFunParams #-}
    toFunParams (a, b) kont =
        toFunParams a $ \fparams_a -> do
        toFunParams b $ \fparams_b -> do
        kont (fparams_a ++ fparams_b)
