import Geoutils
import Color
import Render
import Mathutils
import Listutils
import DrawUtils
import CircleNode
import CirclePacking
import Collider

noderank :: CircleNode Integer -> Integer
noderank node = nodecontent node

fnewrank :: [CircleNode Integer] -> Integer
fnewrank nodes =  1 + (maximum [(noderank node) | node <- nodes])

main = do
     writeFile "redcirclepacking.svg" $ svgCircleColors circlecolors
     where 
         circlecolors = [((nodecircle node),(hsla2rgba (HSLA ((fromIntegral (noderank node))/(fromIntegral maxrank)) 1.0 0.5 1.0))) | node <- newnodes]
	 maxrank      =  maximum [(noderank node) | node <- newnodes]
	 newnodes     = seed0nodes ++ (circlepacking (Collider seed0nodes) rankseeds0 (RatioRadius 0.9) fnewrank 1000)
	 seed0nodes   = (circlenodesfromseeds rankseeds0)
	 rankseeds0   = (seeds0 0)
