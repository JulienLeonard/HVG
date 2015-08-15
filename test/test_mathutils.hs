module Test_Mathutils where

import Mathutils
import Test.HUnit

test_sample   = TestCase (assertEqual "test sample"   0.4                  (sample  (Range 0.0 2.0) 0.2))
test_samples  = TestCase (assertEqual "test samples"  [0.0,1.0,2.0]        (samples (Range 0.0 2.0) 3))
test_geo      = TestCase (assertEqual "test geo"      [1.0,0.5,0.25,0.125] (geo 1.0 0.5 4))
test_symRange = TestCase (assertEqual "test symRange" (Range (-0.25) 0.25) (symRange 0.25))

mathutils_tests = TestList [TestLabel "test_sample"   test_sample,
		            TestLabel "test_samples"  test_samples,
			    TestLabel "test_geo"      test_geo,
			    TestLabel "test_symRange" test_symRange]
