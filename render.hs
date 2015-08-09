module Render where

import Geoutils
import Color

data Viewport = Viewport {viewport_center :: Point, 
                          viewport_size   :: Float } deriving (Show)

data Render = Render {viewport :: Viewport,
                      drawings  :: [Drawing]} deriving (Show)

setviewport :: Render -> Viewport -> Render
setviewport (Render v cps) newv = Render newv cps

data Drawing = Drawing { dpolygon :: Polygon,
                         dcolor   :: RGBA } deriving (Show)

addDrawing :: Render -> Drawing -> Render
addDrawing (Render r drawings) drawing = Render r (drawings ++ [drawing])

viewportFromBBox :: BBox -> Viewport
viewportFromBBox (BBox (Point x1 y1) (Point x2 y2)) = Viewport (Point ((x1+x2)/2.0) ((y1+y2)/2.0))  (max (x2-x1) (y2-y1))

writePoint :: Point -> String 
writePoint (Point x y) = (show x)++","++(show y)++" "

svgStyleFillColor :: RGBA -> String
svgStyleFillColor rgba = "style=\"fill:rgb("++svgrgb++");fill-opacity:"++(show a)++";stroke-width:0\""
	     where
	         svgrgb = rgba2svg rgba
		 a      = coloropacity rgba
	     

writeDrawing :: Drawing -> String 
writeDrawing (Drawing (Polygon p) color) = "<polygon points=\""++(concatMap writePoint p)++ "\" " ++ (svgStyleFillColor color) ++ "/>"


writeDrawings :: [Drawing] -> String 
writeDrawings cps = (concatMap writeDrawing cps)

svgViewport :: Viewport -> [Char]
svgViewport (Viewport (Point xc yc) radius) = "viewBox=\"" ++ show(xc - radius) ++ " " ++ show(yc - radius) ++ " " ++ show(radius * 2.0) ++ " " ++ show(radius * 2.0) ++ "\""

svgFormat :: Render -> [Char]
svgFormat (Render v drawings) = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"50cm\" height=\"50cm\" " ++ (svgViewport v) ++ ">" ++ (writeDrawings drawings) ++ "</svg>"

