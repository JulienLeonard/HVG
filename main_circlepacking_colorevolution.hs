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

fnewhue :: Float -> Float -> Float
fnewhue hue1 hue2 =  0.1 + (max hue1 hue2)

nodehue :: CircleNode Float -> Float
nodehue node = nodecontent node


main = do
     writeFile "circlepacking_colorand.svg" $ svgCircleColors circlecolors
     where 
         circlecolors  = [((nodecircle node),(hsla2rgba (HSLA (nodehue node) 1.0 0.5 1.0))) | node <- newnodes]
	 newnodes      = seed0nodes ++ (circlepacking (Collider seed0nodes) hueseeds0 (RatioRadius 0.9) fnewhue 5000)
	 seed0nodes    = circlenodesfromseeds hueseeds0
	 hueseeds0     = (seeds0 0.0)
