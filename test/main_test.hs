import Test_Geoutils
import Test.HUnit
import System.Exit

main = do
    count <- runTestTT geoutils_tests
    if failures count > 0 then exitFailure else return ()

