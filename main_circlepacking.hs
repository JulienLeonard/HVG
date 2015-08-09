import Geoutils
import Color
import Render
import Mathutils
import Listutils
import DrawUtils
import CircleNode
import CirclePacking
import Collider

main = do
     writeFile "redcirclepacking.svg" $ svgCircleColors circlecolors
     where 
         circlecolors = [((nodecircle node),(hsla2rgba (HSLA ((fromIntegral (noderank node))/(fromIntegral maxrank)) 1.0 0.5 1.0))) | node <- newnodes]
	 maxrank      =  maximum [(noderank node) | node <- newnodes]
	 newnodes     = seed0nodes ++ (circlepacking (Collider seed0nodes) seeds0 (RatioRadius 0.9) 5000)
	 seed0nodes   = (circlenodesfromseeds seeds0)
