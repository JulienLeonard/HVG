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
         circles = [c0,c1] ++ [circles2circle c0 c1 1.0 side | side <- [-1.0,1.0]]
         c0 = (Circle (Point 0.0 0.0) 1.0)
         c1 = (Circle (Point 2.0 0.0) 1.0)
