{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Array.Nikola.Backend.Main
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Backend.Main (
    defaultMain,
    defaultMainWith
  ) where

import Control.Applicative
import Control.Exception (finally)
import Control.Monad (void,
                      when)
import qualified Data.ByteString.Lazy as B
import Data.Monoid
import qualified Data.Text.Lazy.Encoding as E
import System.Console.GetOpt
import System.Environment (getArgs,
                           getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.IO (Handle,
                  IOMode(..),
                  openFile,
                  hClose,
                  stderr,
                  stdout)
import Text.Printf (HPrintfType, hPrintf)
import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Backend.C.Codegen
import Data.Array.Nikola.Backend.Flags

import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Optimize
import Data.Array.Nikola.Language.Reify
import Data.Array.Nikola.Language.Sharing
import Data.Array.Nikola.Language.Syntax hiding (Exp, Var)

-- | Parse a positive number.
pos :: (Num a, Ord a, Read a)
    => String -> (Last a -> Flags) -> String -> IO Flags
pos q f s =
    case reads s of
      [(n,"")] | n >= 0    -> return . f $ ljust n
               | otherwise -> parseError $ q ++ " must be non-negative"
      _                    -> parseError $ "invalid " ++ q ++ " provided"

-- | The standard options accepted on the command line.
defaultOptions :: [OptDescr (IO Flags)]
defaultOptions = [
    Option ['h','?'] ["help"]
          (NoArg (return mempty { fHelp = ljust True }))
          "print help and exit"

  , Option [] ["cuda"]
          (NoArg (return mempty { fDialect = ljust CUDA }))
          "target CUDA"

  , Option [] ["openmp"]
          (NoArg (return mempty { fDialect = ljust OpenMP }))
          "target OpenMP"

  , Option [] ["opencl"]
          (NoArg (return mempty { fDialect = ljust OpenCL }))
          "target OpenCL"

  , Option [] ["no-sharing"]
          (NoArg (return mempty { fObsSharing = ljust False }))
          "do not observe sharing"

 , Option ['O']     []
          (ReqArg (pos "optimization level" $ \n -> mempty { fOptimize = n }) "N")
          "optimize"

 , Option []        ["ppr-cols"]
          (ReqArg (pos "ppr columns" $ \n -> mempty { fPprCols = n }) "N")
          "set number of columns for pretty printing"

 , Option ['V']     ["verbose"]
          (ReqArg (pos "verbosity level" $ \n -> mempty { fVerbosity = n }) "N")
          "verbosity"

 , Option ['o']     ["output"]
          (ReqArg (\path -> return $ mempty { fOutput = ljust path }) "FILENAME")
          "file to write to"

 , Option []        ["func", "function"]
          (ReqArg (\fname -> return $ mempty { fFunction = ljust fname }) "NAME")
          "name of the top-level function being compiled"
 ]

parseArgs :: Flags
          -> [OptDescr (IO Flags)]
          -> [String]
          -> IO (Flags, [String])
parseArgs defFlags options args =
  case getOpt Permute options args of
    (_, _, (err:_)) -> parseError err
    (opts, rest, _) -> do
      f <- (mappend defFlags . mconcat) <$> sequence opts
      if fromLJust fHelp f
        then printUsage options ExitSuccess
        else return (f, rest)

parseError :: String -> IO a
parseError msg = do
  void $ printError "Error: %s" msg
  void $ printError "Run \"%s --help\" for usage information\n" =<< getProgName
  exitWith (ExitFailure 64)

printError :: (HPrintfType r) => String -> r
printError = hPrintf stderr

printUsage :: [OptDescr (IO Flags)] -> ExitCode -> IO a
printUsage options exitCode = do
  p <- getProgName
  putStr (usageInfo ("Usage: " ++ p ++ " [OPTIONS]") options)
  exitWith exitCode

defaultMain :: Reifiable a ProcH => a -> IO ()
defaultMain = defaultMainWith defaultFlags

defaultMainWith :: Reifiable a ProcH => Flags -> a -> IO ()
defaultMainWith f a = do
    args    <- getArgs
    (f', _) <- parseArgs f defaultOptions args

    proc      <- snd <$> runR (reify a >>= detectSharing ProcHA >>= optimize f')
                              (emptyREnv f')
    when (fromLJust fVerbosity f' > 0) $ do
        pprIO f' stderr proc
    code <- compileProgram f' proc
    case fOutput f' of
      Last Nothing     -> pprIO f' stdout code
      Last (Just "-")  -> pprIO f' stdout code
      Last (Just path) -> do  h <- openFile path WriteMode
                              pprIO f' h code `finally` hClose h

optimize :: Flags -> ProcH -> R r ProcH
optimize f p
    | fromLJust fOptimize f > 0 = optimizeHostProgram p
    | otherwise                 = liftHostProgram p

pprIO :: Pretty a => Flags -> Handle -> a -> IO ()
pprIO f h a = do
    B.hPut h (E.encodeUtf8 (prettyLazyText (fromLJust fPprCols f) (ppr a)))
    putStr "\n"
