-- Copyright (c) 2010
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude hiding (map, zipWith)
import qualified Prelude as P

import Control.Monad.Trans
import qualified Data.Packed.Vector as V
import Text.PrettyPrint.Mainland

import Data.Array.Nikola

main :: IO ()
main = withNewContext $ \_ -> do
    test0
    test1
    test2
    test3
    test4
    test5
    test6

test0 :: IO ()
test0 = g 1 >>= print
  where
    f :: Exp Float -> Exp Float
    f x = x

    g = compileIO f

test1 :: IO ()
test1 = do
    print "id"
    cfun <- reify add >>= compileTopFun "f"
    print $ stack (P.map ppr (cfunDefs cfun))

    withCompiledFunction add $ \f -> compileIO f 1 2 >>= print

    let f = compile add
    print $ f 1 2
    print $ f 3 4
  where
    add :: Exp Int -> Exp Int -> Exp Int
    add = (+)

test2 :: IO ()
test2 = do
    print "Exp (Vector Int) -> Exp (Vector Int)"
    cfun <- reify f >>= compileTopFun "f"
    print $ stack (P.map ppr (cfunDefs cfun))

    withCompiledFunction f $ \f -> do
        compileIO f (V.fromList [1..10]) >>= print . V.toList
  where
    f :: Exp (V.Vector Int) -> Exp (V.Vector Int)
    f v = map (\x -> x) v

test3 :: IO ()
test3 = do
    print "Exp [Int] -> Exp [Int]"
    cfun <- reify f >>= compileTopFun "f"
    print $ stack (P.map ppr (cfunDefs cfun))

    withCompiledFunction f $ \f -> do
        compileIO f [1..32] >>= print
  where
    f :: Exp [Int] -> Exp [Int]
    f v = map (\x -> x) v

test4 :: IO ()
test4 = do
    print "Exp (Vector Int) -> Exp [Float]"
    cfun <- reify f >>= compileTopFun "f"
    print $ stack (P.map ppr (cfunDefs cfun))

    withCompiledFunction f $ \f -> do
        compileIO f (V.fromList [1..32]) >>= print
  where
    f :: Exp (V.Vector Int) -> Exp [Float]
    f v = map (\_ -> 1) v

test5 :: IO ()
test5 = do
    print "shared zipWithPlus"
    cfun <- reify f >>= compileTopFun "f"
    print $ stack (P.map ppr (cfunDefs cfun))

    withCompiledFunction f $ \f -> do
        compileIO f [1..32] [1..32] >>= print
  where
    f :: Exp [Float] -> Exp [Float] -> Exp [Float]
    f x y = zipWithPlus temp temp
      where
        zipWithPlus :: Exp [Float] -> Exp [Float] -> Exp [Float]
        zipWithPlus = vapply $ \x y -> zipWith (+) x y

        temp :: Exp [Float]
        temp = zipWithPlus x y

test6 :: IO ()
test6 = do
    print "shared function"
    f'   <- reify f
    liftIO $ print f'
    cfun <- compileTopFun "f" f'
    print $ stack (P.map ppr (cfunDefs cfun))

    withCompiledFunction f $ \f -> do
        compileIO f 2 3 >>= print
  where
    f :: Exp Float -> Exp Float -> Exp Float
    f x y = g x + g y

    g :: Exp Float -> Exp Float
    g = vapply $ \x -> 2 * x
