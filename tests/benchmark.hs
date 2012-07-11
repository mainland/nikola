{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude hiding (map)
import qualified Prelude as P

import Control.Monad
import Criterion.Config
import Criterion.Main
import Criterion.Monad
import Criterion.Plot
import Data.Array.Vector
import Nikola
import Statistics.Function (minMax)
import Statistics.KernelDensity (epanechnikovPDF)
import Statistics.Types
import System.Environment (getArgs)

main :: IO ()
main = do
    (cfg, _) <- parseArgs defaultConfig defaultOptions =<< System.Environment.getArgs
    withNewContext $ \_ -> do
    reify f >>= compileTopFun "f" >>= print
    samples <- withCompiledFunction f $ \f -> do
               replicateM 10000 $ do
               timeKernel f $ \f -> do
               call f [1..1024]
    withConfig cfg $
        plotAll [("map", (toU . P.map (realToFrac . fst)) samples)]
  where
    f :: Exp [Int] -> Exp [Int]
    f = map (\x -> x + 1)

plotAll :: [(String, Sample)] -> Criterion ()
plotAll descTimes = forM_ descTimes $ \(desc,times) -> do
  plotWith Timing $ \o -> plotTiming o desc times
  plotWith KernelDensity $ \o -> uncurry (plotKDE o desc extremes)
                                     (epanechnikovPDF 100 times)
  where
    extremes = case descTimes of
                 (_:_:_) -> toJust . minMax . concatU . P.map snd $ descTimes
                 _       -> Nothing
    toJust r@(lo :*: hi)
        | lo == infinity || hi == -infinity = Nothing
        | otherwise                         = Just r
        where infinity                      = 1/0
