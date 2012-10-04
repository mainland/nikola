module ParseConfig (parseArgs) where

import Control.Applicative
import Control.Monad (void)
import Data.Char (toLower)
import Data.Monoid
import System.Console.GetOpt
import System.Environment (getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.IO (stderr)
import Text.Printf (HPrintfType, hPrintf)

import Config

-- | The standard options accepted on the command line.
defaultOptions :: [OptDescr (IO Config)]
defaultOptions = [
   Option ['h','?'] ["help"]
          (NoArg (return mempty { confHelp = ljust True }))
          "print help and exit"

 , Option [] ["backend"]
          (ReqArg (backend $ \b -> mempty { confBackend = b }) "[repa|repav1|nikola|nikolav1]")
          "back-end (Repa)"

 , Option [] ["benchmark"]
          (NoArg (return mempty { confBench = ljust True }))
          "benchmark instead of displaying animation"

 , Option [] ["size"]
          (ReqArg (pos "size" $ \n -> mempty { confSize = n }) "N")
          "visualisation size (512)"

 , Option [] ["limit"]
          (ReqArg (pos "limit" $ \n -> mempty { confLimit = n }) "N")
          "iteration limit for escape (255)"
 ]

parseArgs :: Config
          -> [String]
          -> IO (Config, [String])
parseArgs conf args =
  case getOpt Permute defaultOptions args of
    (_, _, (err:_)) -> parseError err
    (opts, rest, _) -> do
      conf' <- (mappend conf . mconcat) <$> sequence opts
      if fromLJust confHelp conf'
        then printUsage defaultOptions ExitSuccess
        else return (conf', rest)

parseError :: String -> IO a
parseError msg = do
  void $ printError "Error: %s" msg
  void $ printError "Run \"%s --help\" for usage information\n" =<< getProgName
  exitWith (ExitFailure 64)

printError :: (HPrintfType r) => String -> r
printError = hPrintf stderr

printUsage :: [OptDescr (IO Config)] -> ExitCode -> IO a
printUsage options exitCode = do
  p <- getProgName
  putStr (usageInfo ("Usage: " ++ p ++ " [OPTIONS]") options)
  exitWith exitCode

-- | Parse a backend
backend :: (Last Backend -> Config) -> String -> IO Config
backend f s =
    case map toLower s of
      "repa"     -> return . f $ ljust RepaV2
      "repav1"   -> return . f $ ljust RepaV1
      "repav2"   -> return . f $ ljust RepaV2
      "nikola"   -> return . f $ ljust NikolaV4
      "nikolav1" -> return . f $ ljust NikolaV1
      "nikolav2" -> return . f $ ljust NikolaV2
      "nikolav3" -> return . f $ ljust NikolaV3
      "nikolav4" -> return . f $ ljust NikolaV4
      _          -> parseError $ s ++ " is not a valid back-end"

-- | Parse a positive number.
pos :: (Num a, Ord a, Read a)
    => String -> (Last a -> Config) -> String -> IO Config
pos q f s =
    case reads s of
      [(n,"")] | n >= 0    -> return . f $ ljust n
               | otherwise -> parseError $ q ++ " must be non-negative"
      _                    -> parseError $ "invalid " ++ q ++ " provided"
