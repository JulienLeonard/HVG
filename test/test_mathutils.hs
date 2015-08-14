module Test_Mathutils where

import Mathutils

test_sample  = sample  (Range 0.0 2.0) 0.2
test_samples = samples (Range 0.0 2.0) 4
test_geo = geo 1.0 0.5 10