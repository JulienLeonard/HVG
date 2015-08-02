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


writePoint :: Point -> String 
writePoint (Point x y) = (show x)++","++(show y)++" "

writePolygon :: (Color,Polygon) -> String 
writePolygon ((Color r g b),(Polygon p)) = "<polygon points=\""++(concatMap writePoint p)++"\" style=\"fill:rgb("++(show r)++","++(show g)++","++(show b)++");stroke-width:0\"/>"

writePolygons :: [(Color,Polygon)] -> String 
writePolygons cps = (concatMap writePolygon cps)

svgViewport :: Viewport -> [Char]
svgViewport (Viewport (Point xc yc) radius) = "viewBox=\"" ++ show(xc - radius) ++ " " ++ show(yc - radius) ++ " " ++ show(radius * 2.0) ++ " " ++ show(radius * 2.0) ++ "\""

svgFormat :: Render -> [Char]
svgFormat (Render v cps) = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10cm\" height=\"10cm\" " ++ (svgViewport v) ++ ">" ++ (writePolygons cps) ++ "</svg>"

