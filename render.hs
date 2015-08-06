module Render where

import Geoutils
import Color

data Viewport = Viewport Point Float deriving (Show)

data Render = Render Viewport [(Color,Polygon)] deriving (Show)

viewport :: Render -> Viewport
viewport (Render v _) = v

setviewport :: Render -> Viewport -> Render
setviewport (Render v cps) newv = Render newv cps

colorpolygons :: Render -> [(Color,Polygon)]
colorpolygons (Render _ cps) = cps

addcolorpolygon :: Render -> (Color,Polygon) -> Render
addcolorpolygon (Render r cps) cp = Render r (cps ++ [cp])

viewportFromBBox :: BBox -> Viewport
viewportFromBBox (BBox (Point x1 y1) (Point x2 y2)) = Viewport (Point ((x1+x2)/2.0) ((y1+y2)/2.0))  (max (x2-x1) (y2-y1))

writePoint :: Point -> String 
writePoint (Point x y) = (show x)++","++(show y)++" "

svgStyleFillColor :: Color -> String
svgStyleFillColor color = "style=\"fill:rgb("++svgrgb++");fill-opacity:"++(show a)++";stroke-width:0\""
	     where
	         svgrgb = color2svgrgb color
		 a      = coloropacity color
	     

writePolygon :: (Color,Polygon) -> String 
writePolygon (color,(Polygon p)) = "<polygon points=\""++(concatMap writePoint p)++ "\" " ++ (svgStyleFillColor color) ++ "/>"


writePolygons :: [(Color,Polygon)] -> String 
writePolygons cps = (concatMap writePolygon cps)

svgViewport :: Viewport -> [Char]
svgViewport (Viewport (Point xc yc) radius) = "viewBox=\"" ++ show(xc - radius) ++ " " ++ show(yc - radius) ++ " " ++ show(radius * 2.0) ++ " " ++ show(radius * 2.0) ++ "\""

svgFormat :: Render -> [Char]
svgFormat (Render v cps) = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10cm\" height=\"10cm\" " ++ (svgViewport v) ++ ">" ++ (writePolygons cps) ++ "</svg>"

