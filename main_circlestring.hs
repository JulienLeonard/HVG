import Geoutils
import Color
import Render
import CirclePattern
import Mathutils

main = do
     writeFile "redcircles.svg" $ writePolygons [(red,(circlePolygon c)) | c <- cstring]
     where 
         cstring = (circlestring (Circle (Point 100.0 500.0) 100.0) ras)
	     where
	         ras = (zip [r | r <- (geo 100.0 0.9 100)] [a | a <- (samples rangeangle 100)])
