module Test_CirclePattern where

import CirclePattern
import Geoutils

test_adjcircle = adjcircle (Circle (Point 0.0 0.0) 1.0) 2.0 (3.14159/2.0)
test_circlepattern = circlestring (Circle (Point 0.0 0.0) 1.0) [(1.0,0.1),(1.0,0.2),(1.0,0.3)]