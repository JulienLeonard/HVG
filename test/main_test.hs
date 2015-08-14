import Test_Geoutils
import Test_Mathutils
import Test.HUnit
import System.Exit

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
 
main :: IO ()
main = defaultMain tests
 
tests :: [Test.Framework.Test]
tests =
  [
    testGroup "Geoutils tests" $ hUnitTestToTests geoutils_tests,
    testGroup "Mathutils tests" $ hUnitTestToTests mathutils_tests
  ]

-- main = do
--    count <- runTestTT geoutils_tests 
--    if failures count > 0 then exitFailure else return ()

