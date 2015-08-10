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

main = do
     writeFile "circlepacking_colorand.svg" $ svgCircleColors circlecolors
     where 
         circlecolors  = [((nodecircle node),(hsla2rgba (HSLA hue 1.0 0.5 1.0))) | (node,hue) <- newnodehues]
	 newnodehues   = zip newnodes randhues
	 randhues      = take (length newnodes) $ (randomRs (rangevalues range0) (mkStdGen 0))
	 newnodes      = seed0nodes ++ (circlepacking (Collider seed0nodes) seeds0 (RatioRadius 0.9) 5000)
	 seed0nodes    = circlenodesfromseeds seeds0
