import Geoutils
import Color
import Render
import Mathutils
import Listutils
import DrawUtils
import CirclePacking

main = do
     writeFile "circles2circle.svg" $ svgCircles circles red
     where 
         circles = [c0,c1] ++ [circles2circle (Seed (CirclePair c0 c1) side) (Radius 0.9) | side <- allsides]
         c0 = (Circle (Point 0.0 0.0) (Radius 1.0))
         c1 = (Circle (Point 1.5 0.0) (Radius 0.5))
