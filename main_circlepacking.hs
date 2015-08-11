import Geoutils
import Color
import Render
import Mathutils
import Listutils
import DrawUtils
import CircleNode
import CirclePacking
import Collider

noderank = nodecontent

fnewrank :: [CircleNode Integer] -> (CirclePackingContext Float) -> Integer
fnewrank nodes context =  1 + (maximum [(noderank node) | node <- nodes])

fnewcontext context = context

main = do
     writeFile "redcirclepacking.svg" $ svgCircleColors circlecolors
     where 
         circlecolors = [((nodecircle node),(hsla2rgba (HSLA ((fromIntegral (noderank node))/(fromIntegral maxrank)) 1.0 0.5 1.0))) | node <- newnodes]
	 maxrank      =  maximum [(noderank node) | node <- newnodes]
	 newnodes     = seed0nodes ++ (circlepacking (Collider seed0nodes) rankseeds0 context0 (RatioRadius 0.9) fnewrank fnewcontext 1000)
	 context0     = context00 0.0
	 seed0nodes   = (circlenodesfromseeds rankseeds0)
	 rankseeds0   = (seeds0 0)
