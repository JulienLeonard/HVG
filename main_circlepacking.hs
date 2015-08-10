import Geoutils
import Color
import Render
import Mathutils
import Listutils
import DrawUtils
import CircleNode
import CirclePacking
import Collider

fnewrank :: Integer -> Integer -> Integer
fnewrank rank1 rank2 =  1 + (max rank1 rank2)

noderank :: CircleNode Integer -> Integer
noderank node = nodecontent node

main = do
     writeFile "redcirclepacking.svg" $ svgCircleColors circlecolors
     where 
         circlecolors = [((nodecircle node),(hsla2rgba (HSLA ((fromIntegral (noderank node))/(fromIntegral maxrank)) 1.0 0.5 1.0))) | node <- newnodes]
	 maxrank      =  maximum [(noderank node) | node <- newnodes]
	 newnodes     = seed0nodes ++ (circlepacking (Collider seed0nodes) rankseeds0 (RatioRadius 0.9) fnewrank 1000)
	 seed0nodes   = (circlenodesfromseeds rankseeds0)
	 rankseeds0   = (seeds0 0)
