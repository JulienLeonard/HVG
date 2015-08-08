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
         circles    = (nodescircles (nodepairnodes circlenodepair0)) ++ newcircles
	 newcircles = (nodescircles (circlepacking (circlesfromseeds seeds0) seeds0 (RatioRadius 0.9) 1000))
