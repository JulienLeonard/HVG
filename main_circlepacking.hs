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
         circlecolors = [((nodecircle node),(hsla2color (((fromIntegral (noderank node))/maxrank), 1.0, 0.5, 1.0))) | node <- newnodes]
	 maxrank      = fromIntegral (maximum [(noderank node) | node <- newnodes])
	 newnodes     = (circlesnodesfromseeds seeds0) ++ (circlepacking (circlesfromseeds seeds0) seeds0 (RatioRadius 0.9) 1000)
