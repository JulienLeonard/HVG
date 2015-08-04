module DrawUtils where

import Geoutils
import Color
import Render
import CirclePattern
import Mathutils
import Listutils

svgPolygons :: [Polygon] -> Color -> String
svgPolygons polygons color = svgFormat (Render (viewportFromBBox (bboxPolygons polygons)) [(color,poly) | poly <- polygons])

svgCircles :: [Circle] -> Color -> String
svgCircles circles color = svgPolygons (map circlePolygon circles) color
