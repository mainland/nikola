{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Main
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Main where

import Prelude hiding (map, zipWith, zipWith3)
import qualified Prelude as P

import Control.Monad
import qualified Criterion as C
import qualified Criterion.Main as C
import Data.Int
import System.Environment
import Text.Printf

import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Backend.CUDA.TH as NTH
import Data.Array.Nikola.Util.Statistics

import qualified Data.Vector.CUDA.Storable as CV
import qualified Data.Vector.Storable as V

import qualified American.Nikola as AMN
import qualified American.Vector as AMV

type F = Double

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    ["--validate"] -> validate
    _              -> C.defaultMain $
                      P.map benchmarksForN [1, 8, 16, 30, 64, 128]

benchmarksForN :: Int -> C.Benchmark
benchmarksForN n =
    C.bgroup (show n)
         [ C.bench (printf "               vector %-3d" n) $
                   C.nf AMV.binom n
         , C.bench (printf "nikola v1 interpreter %-3d" n) $
                   C.nf AMN.binom n
         , C.bench (printf "   nikola v1 compiled %-3d" n) $
                   C.nf AMN.binomCompiled n
         ]

validate :: IO ()
validate =
    forM_ [1, 8, 16, 30, 64, 128] $ \n -> do
      let v1 = AMV.binom n
      let v2 = AMN.binomCompiled n
      validateL1Norm ePSILON (printf "expiry %-3d" n) (V.singleton v1) (V.singleton v2)
  where
    ePSILON :: F
    ePSILON = 1e-10
