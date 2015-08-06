module Color where

data Color = Color Float Float Float Float deriving (Show)

white = (Color 1.0 1.0 1.0 1.0)
red   = (Color 1.0 0.0 0.0 1.0)

coloropacity :: Color -> Float
coloropacity (Color _ _ _ a) = a

float2int255 :: Float -> Integer
float2int255 v = round (255.0 * v)

color2svgrgb :: Color -> String
color2svgrgb (Color r g b _) =  (show ir)++","++(show ig)++","++(show ib)
	     where 
	          ir = float2int255 r
	     	  ig = float2int255 g
		  ib = float2int255 b