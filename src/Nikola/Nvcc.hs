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

module Nikola.Nvcc (
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
             | Fatbin
             | Gencode10
             | Gencode20
             | Debug
             | Opt String
             | Freeform String
  deriving (Eq, Ord)

opts2args :: NvccOpt -> [String]
opts2args Ptx            = ["--ptx"]
opts2args Fatbin         = ["--fatbin"]
opts2args Gencode10      = ["-gencode", "arch=compute_10,code=sm_10"]
opts2args Gencode20      = ["-gencode", "arch=compute_20,code=sm_20"]
opts2args Debug          = ["-G"]
opts2args (Opt lvl)      = ["-O"++lvl]
opts2args (Freeform opt) = [opt]

compileEx :: [NvccOpt] -> [C.Definition] -> IO B.ByteString
compileEx opts cdefs = do
    writeFile cupath (show (stack (map ppr cdefs)))
    (exitCode, _, err) <- readProcessWithExitCode NVCC
        (["--compiler-bindir", NVCC_CC] ++
         concatMap opts2args opts ++
         ["temp.cu"])
        ""
    when (exitCode /= ExitSuccess) $
        fail $ "nvcc failed: " ++ err
    B.readFile objpath
  where
    cupath :: FilePath
    cupath = "temp.cu"

    objpath :: FilePath
    objpath | Fatbin `elem` opts  = "temp.fatbin"
            | otherwise           = "temp.ptx"

compile :: [C.Definition] -> IO B.ByteString
compile = compileEx [Fatbin, Gencode10, Gencode20, Opt ""]

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
