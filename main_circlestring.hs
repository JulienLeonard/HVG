import Geoutils
import Color
import Render
import CirclePattern
import Mathutils

main = do
     writeFile "redcircles.svg" $ writePolygons [(red,(circlePolygon c)) | c <- circlestring [] (Circle (Point 100.0 500.0) 100.0) [(r,0.0) | r <- (geo 100.0 0.5 10)]]
