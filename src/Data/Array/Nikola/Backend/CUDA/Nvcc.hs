{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Array.Nikola.Backend.CUDA.Nvcc
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Backend.CUDA.Nvcc (
    compile
  ) where

import Control.Monad.State
import qualified Data.ByteString as B
import qualified Language.C as C
import System.Exit
import Text.PrettyPrint.Mainland

#if MIN_VERSION_process(1,1,0)
import System.Process (readProcessWithExitCode)
#else /* !MIN_VERSION_process(1,1,0) */
import Control.Concurrent (forkIO,
                           newEmptyMVar,
                           putMVar,
                           takeMVar)
import qualified Control.Exception as E
import System.Process (StdStream(..),
                       proc,
                       createProcess,
                       waitForProcess,
                       std_in,
                       std_out,
                       std_err)
import System.IO (hClose,
                  hFlush,
                  hGetContents,
                  hPutStr)
#endif /* !MIN_VERSION_process(1,1,0) */

#include "Nikola.h"

data NvccOpt = Ptx
             | Cubin
             | Fatbin
             | Gencode10
             | Gencode11
             | Gencode20
             | Gencode30
             | Debug
             | Freeform String
  deriving (Eq, Ord)

opts2args :: NvccOpt -> [String]
opts2args Ptx            = ["--ptx"]
opts2args Fatbin         = ["--fatbin"]
opts2args Cubin          = ["--cubin"]
opts2args Gencode10      = ["-gencode", "arch=compute_10,code=sm_10"]
opts2args Gencode11      = ["-gencode", "arch=compute_11,code=sm_11"]
opts2args Gencode20      = ["-gencode", "arch=compute_20,code=sm_20"]
opts2args Gencode30      = ["-gencode", "arch=compute_30,code=sm_30"]
opts2args Debug          = ["-G"]
opts2args (Freeform opt) = [opt]

compileEx :: [NvccOpt] -> [C.Definition] -> IO B.ByteString
compileEx opts cdefs = do
    writeFile cupath (show (stack (map ppr cdefs)))
    (exitCode, _, err) <- readProcessWithExitCode NVCC
        (["--compiler-bindir", NVCC_CC] ++
         concatMap opts2args opts ++
         [cupath, "-o", objpath])
        ""
    when (exitCode /= ExitSuccess) $
        fail $ "nvcc failed: " ++ err
    B.readFile objpath
  where
    pathRoot :: FilePath
    pathRoot = "temp"

    cupath :: FilePath
    cupath = pathRoot ++ ".cu"

    objpath :: FilePath
    objpath | Fatbin `elem` opts  = pathRoot ++ ".fatbin"
            | Cubin `elem` opts   = pathRoot ++ ".cubin"
            | otherwise           = pathRoot ++ ".ptx"

-- Starting with version 4.0, nvcc can compile a fat binary.
compile :: [C.Definition] -> IO B.ByteString
compile = compileEx
#if NVCC_VERSION < 40
#error "nvcc < 4 not supported"
#elif NVCC_VERSION < 42
    [Fatbin, Gencode11, Gencode20]
#else /* NVCC_VERSION >= 42 */
    [Fatbin, Gencode11, Gencode20, Gencode30]
#endif /* NVCC_VERSION >= 42 */

#if !MIN_VERSION_process(1,1,0)
readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }

    outMVar <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    out  <- hGetContents outh
    _ <- forkIO $ E.evaluate (length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    err  <- hGetContents errh
    _ <- forkIO $ E.evaluate (length err) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh
    hClose errh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, out, err)
#endif /* !MIN_VERSION_process(1,1,0) */
