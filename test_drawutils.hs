module Test_DrawUtils where

import Geoutils
import Color
import DrawUtils

test_svgPolygons = svgPolygons [(Polygon [(Point 0.0 0.0),(Point 1.0 1.0)]),(Polygon [(Point 2.0 2.0),(Point 3.0 3.0)])] red
test_svgCircles = svgCircles [(Circle (Point 0.0 0.0) 1.0),(Circle (Point 1.0 1.0) 2.0)] red