import Geoutils
import Color
import Render
import CirclePacking
import Mathutils
import Listutils
import DrawUtils

main = do
     writeFile "redcirclepacking.svg" $ svgCircles circles red
     where 
         circles    = [(fst circlepair0),(snd circlepair0)] ++ newcircles
	 newcircles = circlepacking (circlesfromseeds seeds0) seeds0 0.9 1000
