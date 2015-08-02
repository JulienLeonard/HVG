import Geoutils
import Color
import Render
import CirclePattern
import Mathutils
import Listutils

main = do
     writeFile "redcircles.svg" $ svgFormat (Render (Viewport p0 1000) [(red,(circlePolygon c)) | c <- cstring])
     where 
         cstring = (circlestring (Circle (Point 100.0 500.0) 100.0) ras)
	     where
	     	ras = lzip2 (map ($ 100) [(geo 100.0 0.9),(samples rangeangle)])
