import Geoutils
import Color
import Render
import CirclePattern

main = do
     writeFile "redcircles.svg" $ writePolygons [(red,(circlePolygon c)) | c <- circlestring [] (Circle (Point 100.0 500.0) 100.0) [(200.0,0.0),(300.0,0.0)]]
