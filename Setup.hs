import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System.FilePath
import System.Process (system)

main :: IO ()
main = defaultMainWithHooks autoconfUserHooks
