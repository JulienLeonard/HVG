module DrawUtils where

import Geoutils
import Color
import Render
import CirclePattern
import Mathutils
import Listutils

svgPolygons :: [Polygon] -> Color -> String
svgPolygons polygons color = svgFormat (Render (viewportFromBBox (bboxPolygons polygons)) [(color,poly) | poly <- polygons])

-- TODO : create class to hold Color and Polygon
svgPolygonColors :: [(Color,Polygon)] -> String
svgPolygonColors cps = svgFormat (Render (viewportFromBBox (bboxPolygons [snd cp | cp <- cps])) cps)


svgCircles :: [Circle] -> Color -> String
svgCircles circles color = svgPolygons (map circlePolygon circles) color


svgCircleColors :: [(Circle,Color)] -> String
svgCircleColors ccs = svgPolygonColors [(color,(circlePolygon circle)) | (circle,color) <- ccs]
