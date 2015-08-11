import Geoutils
import Color
import Render
import Mathutils
import Listutils
import DrawUtils
import CircleNode
import CirclePacking
import Collider
import System.Random

nodehue :: CircleNode (Float,Float) -> Float
nodehue node = fst (nodecontent node)

nodeincr :: CircleNode (Float,Float) -> Float
nodeincr node = snd (nodecontent node)

fnewcontent :: [CircleNode (Float,Float)] -> (Float,Float)
fnewcontent nodes =  (maxincr + (sum([nodehue node | node <- nodes])/fromIntegral(length(nodes))), maxincr)
	    where 
	        maxincr = maximum([nodeincr node | node <- nodes]) + 0.1




main = do
     writeFile "circlepacking_colorand.svg" $ svgCircleColors circlecolors
     where 
         circlecolors  = [((nodecircle node),(hsla2rgba (HSLA (nodehue node) 1.0 0.5 1.0))) | node <- newnodes]
	 newnodes      = seed0nodes ++ (circlepacking (Collider seed0nodes) hueseeds0 (RatioRadius 0.9) fnewcontent 5000)
	 seed0nodes    = circlenodesfromseeds hueseeds0
	 hueseeds0     = (seeds00 (0.0,-0.1) (0.5,0.1))
	 
