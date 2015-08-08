import Geoutils
import Color
import Render
import CirclePacking
import Mathutils
import Listutils
import DrawUtils

main = do
     writeFile "redcirclepacking.svg" $ svgCircleColors circlecolors
     where 
         circlecolors = zip (nodescircles newnodes) colors
	 colors       = [if (noderank node) `mod` 2 == 0 then red else black | node <- newnodes]
	 newnodes     = (circlepacking (circlesfromseeds seeds0) seeds0 (RatioRadius 0.9) 1000)
