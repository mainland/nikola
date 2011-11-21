-- Copyright (c) 2009-2010
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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Nikola.Exec (
    ExState(..),
    emptyExState,
    Ex(..),
    evalEx,
    runEx,

    Arg(..),
    Alloc(..),

    alloc,
    offset,
    pushArg,
    lookupArg,
    pushParam,
    pushAlloc,
    popAlloc,

    putRawLaunch,
    launchKernel,
    defaultLaunch,

    F(..),
    castF,

    timeKernel
  ) where

import CUDA.Event
import CUDA.Internal
import CUDA.Storable
import CUDA.Stream
import Control.Applicative
import Control.Monad.State
import Text.PrettyPrint.Mainland

import Nikola.Syntax

data Arg = ScalarArg
         | VectorArg Int
         | MatrixArg Int Int Int
  deriving (Eq, Ord, Show)

instance Pretty Arg where
    ppr = text . show

data Alloc = ScalarAlloc { allocPtr       :: CUDevicePtr () }
           | VectorAlloc { allocPtr       :: CUDevicePtr ()
                         , allocVecLength :: N
                         }
           | MatrixAlloc { allocPtr       :: CUDevicePtr ()
                         , allocMatStride :: N
                         , allocMatRows   :: N
                         , allocMatCols   :: N
                         }

-- | An execution context for a GPU-embedded function.
data ExState = ExState
    {  exFun       :: CUFunction -- ^ The CUDA function
    ,  exOff       :: Int        -- ^ The current parameter offset
    ,  exArgs      :: [Arg]      -- ^ Arguments
    ,  exAllocs    :: [Alloc]    -- ^ Allocations
    ,  exRawLaunch :: CUFunction -> Int -> Int -> IO ()
    ,  exLaunch    :: forall a . Ex a -> Ex a
    }

emptyExState :: ExState
emptyExState = ExState
    {  exFun       = error "No function specified for execution"
    ,  exOff       = 0
    ,  exArgs      = []
    ,  exAllocs    = []
    ,  exRawLaunch = cuLaunchGrid
    ,  exLaunch    = error "No launch function specified for execution"
    }

newtype Ex a = Ex { unEx :: StateT ExState IO a }
  deriving (Monad,
            MonadState ExState,
            MonadIO)

instance Functor Ex where
    fmap = liftM

instance Applicative Ex where
    pure  = return
    (<*>) = ap

instance MonadEvalN Int Ex where
    evalN = go
      where
        go :: N -> Ex Int
        go n@(NVecLength i) = lookupArg i >>= arg n
        go n@(NMatStride i) = lookupArg i >>= arg n
        go n@(NMatRows i)   = lookupArg i >>= arg n
        go n@(NMatCols i)   = lookupArg i >>= arg n

        go (N i)        = pure i
        go (NAdd n1 n2) = (+) <$> evalN n1 <*> evalN n2
        go (NSub n1 n2) = (-) <$> evalN n1 <*> evalN n2
        go (NMul n1 n2) = (*) <$> evalN n1 <*> evalN n2
        go (NNegate n)  = negate <$> evalN n

        go (NDiv n1 n2)  = div <$> evalN n1 <*> evalN n2
        go (NMod n1 n2)  = mod <$> evalN n1 <*> evalN n2

        go (NMin ns) = minimum <$> mapM evalN ns
        go (NMax ns) = maximum <$> mapM evalN ns

        arg :: N -> Arg -> Ex Int
        arg (NVecLength _) (VectorArg n)      = return n
        arg (NMatStride _) (MatrixArg n _ _)  = return n
        arg (NMatRows _)   (MatrixArg _ n _)  = return n
        arg (NMatCols _)   (MatrixArg _ _ n ) = return n
        arg n              arg                = faildoc $
                                                text "Argument" <+> ppr arg <+>
                                                text "does not match index" <+>
                                                ppr n

evalEx  ::  Ex a
        ->  ExState
        ->  IO a
evalEx m s = evalStateT (unEx m) s

runEx  ::  Ex a
       ->  ExState
       ->  IO (a, ExState)
runEx m s = runStateT (unEx m) s

offset :: Storable a => a -> Ex ()
offset x = modify $ \s ->
    s { exOff = exOff s + sizeOf x }

getFun :: Ex CUFunction
getFun = gets exFun

getOffset :: Ex Int
getOffset = gets exOff

pushArg :: Arg -> Ex ()
pushArg arg = modify $ \s ->
    s { exArgs = exArgs s ++ [arg] }

lookupArg :: Int -> Ex Arg
lookupArg i = do
    args <- gets exArgs
    return (args !! i)

getAllocs :: Ex [Alloc]
getAllocs = gets exAllocs

pushAlloc :: Alloc -> Ex ()
pushAlloc alloc = modify $ \s ->
    s { exAllocs = alloc : exAllocs s }

popAlloc :: Ex Alloc
popAlloc = do
    (a : as) <- gets exAllocs
    modify $ \s -> s { exAllocs = as }
    return a

getRawLaunch :: Ex (CUFunction -> Int -> Int -> IO ())
getRawLaunch = gets exRawLaunch

putRawLaunch :: (CUFunction -> Int -> Int -> IO ()) -> Ex ()
putRawLaunch rawLaunch = modify $ \s ->
    s { exRawLaunch = rawLaunch }

launchKernel :: Ex a -> Ex a
launchKernel act = do
    l <- gets exLaunch
    l act

pushParam :: Storable a => a -> Ex ()
pushParam x = do
    f    <- getFun
    off  <- getOffset
    off' <- liftIO $ setParam f off x
    modify $ \s -> s { exOff = off' }

alloc :: Rho -> Ex ()
alloc (ScalarT tau) = do
    devPtr :: CUDevicePtr Int <- liftIO $ cuMemAlloc (sizeOfTau tau)
    pushAlloc (ScalarAlloc (castCUDevicePtr devPtr))
    pushParam devPtr

alloc (VectorT tau n) = do
    count         <- evalN n
    let byteCount =  if count == 0 then 1 else count * sizeOfTau tau
    devPtr :: CUDevicePtr Int <- liftIO $ cuMemAlloc byteCount
    pushAlloc (VectorAlloc (castCUDevicePtr devPtr) n)
    pushParam devPtr
    countDevPtr :: CUDevicePtr Int <- liftIO $ cuMemAlloc (sizeOfTau IntT)
    pushAlloc (ScalarAlloc (castCUDevicePtr countDevPtr))
    pushParam countDevPtr

alloc (MatrixT tau s r c) = do
    count         <- (*) <$> evalN r <*> evalN s
    let byteCount =  count * sizeOfTau tau
    devPtr :: CUDevicePtr Int <- liftIO $ cuMemAlloc byteCount
    pushAlloc (MatrixAlloc (castCUDevicePtr devPtr) s r c)
    pushParam (devPtr :: CUDevicePtr Int)

alloc tau@(FunT {}) =
    faildoc $ text "Cannot allocate type" <+> ppr tau

defaultLaunch :: Int -> Int -> Int
              -> Int -> Int
              -> Ex a
              -> Ex a
defaultLaunch dimX dimY dimZ gridW gridH act = do
    -- liftIO $ print (dimX, dimY, dimZ, gridW, gridH)
    f         <- getFun
    off       <- getOffset
    rawLaunch <- getRawLaunch
    liftIO $ cuParamSetSize f off
    liftIO $ cuFuncSetBlockShape f dimX dimY dimZ
    liftIO $ rawLaunch f gridW gridH
    x <- act
    getAllocs >>= liftIO . mapM_ (cuMemFree . allocPtr)
    return x

-- | A wrapping of a GPU execution environment that provides a phantom type
-- parameter.
newtype F a = F { unF :: ExState }

castF :: F a -> F b
castF = F . unF

timeKernel :: F a
           -> (F a -> IO b)
           -> IO (Double, b)
timeKernel f act = do
    withNewStream $ \stream -> do
    withNewEvent $ \start -> do
    withNewEvent $ \end -> do
    x <- act (timedLaunch stream start end f)
    t <- ((/ 1000.0) . realToFrac) <$> eventElapsedTime start end
    return (t, x)
  where
    timedLaunch :: Stream -> Event -> Event -> F a -> F a
    timedLaunch stream start end f = F ((unF f) { exRawLaunch = launch})
      where
        launch :: CUFunction -> Int -> Int -> IO ()
        launch f gridW gridH = do
            recordEvent start stream
            cuLaunchGridAsync f gridW gridH stream
            recordEvent end stream
            synchronizeEvent end
