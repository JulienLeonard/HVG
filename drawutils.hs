module DrawUtils where

import Geoutils
import Color
import Render
import CirclePattern
import Mathutils
import Listutils


svgPolygons :: [Polygon] -> Color -> String
svgPolygons polygons color = svgFormat (Render (viewportFromBBox (bboxPolygons polygons)) [(Drawing poly color) | poly <- polygons])

-- TODO : create class to hold Color and Polygon
svgPolygonColors :: [Drawing] -> String
svgPolygonColors cps = svgFormat (Render (viewportFromBBox (bboxPolygons [dpolygon cp | cp <- cps])) cps)


svgCircles :: [Circle] -> Color -> String
svgCircles circles color = svgPolygons (map circlePolygon circles) color


svgCircleColors :: [(Circle,Color)] -> String
svgCircleColors ccs = svgPolygonColors [(Drawing (circlePolygon circle) color) | (circle,color) <- ccs]
