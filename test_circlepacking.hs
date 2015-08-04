module Test_CirclePacking where

import CirclePacking
import Geoutils

test_circles2circle = circles2circle (Circle (Point 0.0 0.0) 1.0) (Circle (Point 2.0 0.0) 1.0) 1.0 (-1.0)
