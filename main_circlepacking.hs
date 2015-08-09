import Geoutils
import Color
import Render
import CirclePacking
import Mathutils
import Listutils
import DrawUtils
import Collider

main = do
     writeFile "redcirclepacking.svg" $ svgCircleColors circlecolors
     where 
         circlecolors = [((nodecircle node),(hsla2rgba (HSLA ((fromIntegral (noderank node))/(fromIntegral maxrank)) 1.0 0.5 1.0))) | node <- newnodes]
	 maxrank      =  maximum [(noderank node) | node <- newnodes]
	 newnodes     = (circlesnodesfromseeds seeds0) ++ (circlepacking (Collider (circlesfromseeds seeds0)) seeds0 (RatioRadius 0.9) 5000)
