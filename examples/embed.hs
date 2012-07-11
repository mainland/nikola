{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad
import Nikola

main :: IO ()
main = withNewContext $ \_ -> do
    withCompiledFunction f $ \f -> replicateM_ 50 $ do
        call f 1 >>= print
        call f 10 >>= print
    call g 10 20 >>= print
  where
    f :: Exp Int -> Exp Int
    f x = x

    g :: Exp Float -> Exp Float -> Exp Float
    g x y = x + y
