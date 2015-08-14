import Geoutils
import Color
import Render

main = do
     writeFile "redcircle.svg" $ writePolygons [(red,(circlePolygon (Circle (Point 500.0 500.0) 100.0)))]


