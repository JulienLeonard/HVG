module Test_CirclePacking where

import CirclePacking
import Geoutils

test_circles2circle = circles2circle c1 c2 SideLeft (Radius 1.0)
		    where 
		         [c1,c2] = (nodescircles (nodepairnodes circlenodepair0))
test_circlepacking  = circlepacking (circlesfromseeds seeds0) seeds0 (RatioRadius 0.9) 1000
