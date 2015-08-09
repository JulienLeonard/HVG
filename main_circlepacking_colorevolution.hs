import Geoutils
import Color
import Render
import Mathutils
import Listutils
import DrawUtils
import CircleNode
import CirclePacking
import Collider

nodeHue :: CircleNode -> Float -> Float
nodeHue (CircleNode _ 0 _) _ = 0.0
nodeHue (CircleNode _ _ (prev:prevs)) incrhue = (nodeHue prev incrhue) + incrhue

main = do
     writeFile "circlepacking_colorevolution.svg" $ svgCircleColors circlecolors
     where 
         circlecolors = [((nodecircle node),(hsla2rgba (HSLA (nodeHue node 0.1) 1.0 0.5 1.0))) | node <- newnodes]
	 newnodes     = seed0nodes ++ (circlepacking (Collider seed0nodes) seeds0 (RatioRadius 0.9) 5000)
	 seed0nodes   = (circlenodesfromseeds seeds0)
