module Test_Mathutils where

import Mathutils
import Test.HUnit

test_sample  = TestCase (assertEqual "test sample" 0.4 (sample  (Range 0.0 2.0) 0.2))
-- test_samples = samples (Range 0.0 2.0) 4
-- test_geo = geo 1.0 0.5 10

mathutils_tests = TestList [TestLabel "test_sample"  test_sample]
