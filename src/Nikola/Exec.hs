-- Copyright (c) 2009-2012
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
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Nikola.Exec (
    ExState(..),
    emptyExState,
    Ex(..),
    evalEx,
    runEx,

    Arg(..),
    Alloc(..),

    alloc,
    pushArg,
    lookupArg,
    pushAlloc,
    popAlloc,

    launchKernel,
    defaultLaunch,

    F(..),
    castF,

    timeKernel
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad.State
import qualified Foreign.CUDA.Driver as CU
import qualified Foreign.CUDA.Driver.Event as CU (Event)
import qualified Foreign.CUDA.Driver.Event as CUEvent
import qualified Foreign.CUDA.Driver.Stream as CU (Stream)
import qualified Foreign.CUDA.Driver.Stream as CUStream
import Text.PrettyPrint.Mainland

import Nikola.Syntax

data Arg = IntArg    !Int
         | FloatArg  !Float
         | VectorArg !Int                -- ^ Size of the vector
                     !(CU.DevicePtr ())  -- ^ The vector
         | MatrixArg !Int                -- ^ Stride
                     !Int                -- ^ Rows
                     !Int                -- ^ Columns
                     !(CU.DevicePtr ())  -- ^ The matrix
         | PtrArg    !(CU.DevicePtr ())  -- ^ Used to push allocations
  deriving (Eq, Ord, Show)

instance Pretty Arg where
    ppr = text . show

data Alloc = IntAlloc    { allocPtr       :: CU.DevicePtr () }
           | FloatAlloc  { allocPtr       :: CU.DevicePtr () }
           | VectorAlloc { allocVecLength :: N
                         , allocPtr       :: CU.DevicePtr ()
                         }
           | MatrixAlloc { allocMatStride :: N
                         , allocMatRows   :: N
                         , allocMatCols   :: N
                         , allocPtr       :: CU.DevicePtr ()
                         }
  deriving (Eq, Ord, Show)

-- | An execution context for a GPU-embedded function.
data ExState = ExState
    {  exFun     :: CU.Fun     -- ^ The CUDA function
    ,  exArgs    :: [Arg]      -- ^ Arguments
    ,  exAllocs  :: [Alloc]    -- ^ Allocations
    ,  exStream  :: Maybe CU.Stream
    ,  exLaunch  :: forall a . Ex a -> Ex a
    }

emptyExState :: ExState
emptyExState = ExState
    {  exFun     = error "No function specified for execution"
    ,  exArgs    = []
    ,  exAllocs  = []
    ,  exStream  = Nothing
    ,  exLaunch  = error "No launch function specified for execution"
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
        arg (NVecLength _) (VectorArg n _)      = return n
        arg (NMatStride _) (MatrixArg n _ _ _)  = return n
        arg (NMatRows _)   (MatrixArg _ n _ _)  = return n
        arg (NMatCols _)   (MatrixArg _ _ n _)  = return n
        arg n              arg                  = faildoc $
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

getFun :: Ex CU.Fun
getFun = gets exFun

getArgs :: Ex [Arg]
getArgs = gets exArgs

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

getStream :: Ex (Maybe CU.Stream)
getStream = gets exStream

launchKernel :: Ex a -> Ex a
launchKernel act = do
    l <- gets exLaunch
    l act

allocScalar :: Tau -> Ex ()
allocScalar UnitT =
    faildoc $ text "Cannot allocate type" <+> ppr UnitT

allocScalar BoolT = do
    devPtr :: CU.DevicePtr Int <- liftIO $ CU.mallocArray 1
    pushAlloc (IntAlloc (CU.castDevPtr devPtr))
    pushArg (PtrArg (CU.castDevPtr devPtr))

allocScalar IntT = do
    devPtr :: CU.DevicePtr Int <- liftIO $ CU.mallocArray 1
    pushAlloc (IntAlloc (CU.castDevPtr devPtr))
    pushArg (PtrArg (CU.castDevPtr devPtr))

allocScalar FloatT = do
    devPtr :: CU.DevicePtr Float <- liftIO $ CU.mallocArray 1
    pushAlloc (FloatAlloc (CU.castDevPtr devPtr))
    pushArg (PtrArg (CU.castDevPtr devPtr))

alloc :: Rho -> Ex ()
alloc (VectorT IntT n) = do
    count       <-  evalN n
    let count'  =   max 1 count
    -- Push vector data
    devPtr :: CU.DevicePtr Int <- liftIO $ CU.mallocArray count'
    pushAlloc (VectorAlloc n (CU.castDevPtr devPtr))
    pushArg (PtrArg (CU.castDevPtr devPtr))
    -- Push vector size
    allocScalar IntT

alloc (VectorT FloatT n) = do
    count       <-  evalN n
    let count'  =   max 1 count
    -- Push vector data
    devPtr :: CU.DevicePtr Float <- liftIO $ CU.mallocArray count'
    pushAlloc (VectorAlloc n (CU.castDevPtr devPtr))
    pushArg (PtrArg (CU.castDevPtr devPtr))
    -- Push vector size
    allocScalar IntT

alloc (MatrixT IntT s r c) = do
    count <- (*) <$> evalN s <*> evalN c
    -- Push vector data
    devPtr :: CU.DevicePtr Int <- liftIO $ CU.mallocArray count
    pushAlloc (MatrixAlloc s r c (CU.castDevPtr devPtr))
    pushArg (PtrArg (CU.castDevPtr devPtr))

alloc (MatrixT FloatT s r c) = do
    count <- (*) <$> evalN s <*> evalN c
    -- Push vector data
    devPtr :: CU.DevicePtr Float <- liftIO $ CU.mallocArray count
    pushAlloc (MatrixAlloc s r c (CU.castDevPtr devPtr))
    pushArg (PtrArg (CU.castDevPtr devPtr))

alloc (ScalarT tau) =
    allocScalar tau

alloc rho =
    faildoc $ text "Cannot allocate type" <+> ppr rho

instance Show CU.FunParam where
    show (CU.IArg n)  = "IArg " ++ show n
    show (CU.FArg n)  = "FArg " ++ show n
    show (CU.TArg {}) = "TArg"
    show (CU.VArg {}) = "VArg"

defaultLaunch :: Int -> Int -> Int
              -> Int -> Int
              -> Ex a
              -> Ex a
defaultLaunch dimX dimY dimZ gridW gridH act = do
    --liftIO $ print (dimX, dimY, dimZ, gridW, gridH)
    f           <-  getFun
    args        <-  getArgs
    stream      <-  getStream
    let params  =   concatMap arg2params args
{-
    liftIO $ putStrLn $ "              args: " ++ show args
    liftIO $ putStrLn $ "            params: " ++ show params
    liftIO $ putStrLn $ "(dimX, dimY, dimZ): " ++ show (dimX, dimY, dimZ)
    liftIO $ putStrLn $ " (gridW, gridH, 1): " ++ show (gridW, gridH, 1)
-}

    liftIO $ CU.launchKernel f  (gridW, gridH, 1) (dimX, dimY, dimZ)
                                0 stream params
    x <- act
    getAllocs >>= liftIO . mapM_ (CU.free . allocPtr)
    return x
  where
    arg2params :: Arg -> [CU.FunParam]
    arg2params (IntArg i)             = [CU.IArg i]
    arg2params (FloatArg f)           = [CU.FArg f]
    arg2params (VectorArg n ptr)      = [CU.VArg ptr, CU.IArg n]
    arg2params (MatrixArg s r c ptr)  = [CU.VArg ptr, CU.IArg s, CU.IArg r, CU.IArg c]
    arg2params (PtrArg ptr)           = [CU.VArg ptr]

-- | A wrapping of a GPU execution environment that provides a phantom type
-- parameter.
newtype F a = F { unF :: ExState }

castF :: F a -> F b
castF = F . unF

withNewStream :: (CU.Stream -> IO a)
              -> IO a
withNewStream kont = do
    stream <- CUStream.create []
    kont stream `finally` CUStream.destroy stream

withNewEvent :: (CU.Event -> IO a)
              -> IO a
withNewEvent kont = do
    stream <- CUEvent.create []
    kont stream `finally` CUEvent.destroy stream

timeKernel :: F a
           -> (F a -> IO b)
           -> IO (Double, b)
timeKernel f act = do
    withNewStream $ \stream -> do
    withNewEvent $ \start -> do
    withNewEvent $ \end -> do
    CUEvent.record start (Just stream)
    CUEvent.block start
    x <- act (setStream stream f)
    CUEvent.record end (Just stream)
    CUEvent.block end
    t <- ((/ 1000.0) . realToFrac) <$> CUEvent.elapsedTime start end
    return (t, x)
  where
    setStream :: CU.Stream -> F a -> F a
    setStream stream f =
        F ((unF f) { exStream = Just stream})
