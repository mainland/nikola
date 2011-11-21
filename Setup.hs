import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System.FilePath
import System.Process (system)

main = defaultMainWithHooks hooks
  where
    hooks = simpleUserHooks { runTests = runTests' }

runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' _ _ _ lbi = system unit_test >> return ()
  where
    unit_test = buildDir lbi </> "unit-test" </> "unit-test"
