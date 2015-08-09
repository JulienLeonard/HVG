module Color where

import Data.Fixed

data Color = Color Float Float Float Float deriving (Show)

white = (Color 1.0 1.0 1.0 1.0)
red   = (Color 1.0 0.0 0.0 1.0)
black = (Color 0.0 0.0 0.0 1.0)

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


_v :: Float -> Float -> Float -> Float
_v m1 m2 hue = if (hueb < (1.0/6.0)) then (m1+(m2-m1)*hueb*6.0) else
      	       	  if (hueb < 0.5) then m2 else
		     	  if (hueb < (2.0/3.0)) then (m1 + (m2-m1)*(2.0/3.0-hueb) * 6.0) else m1
	       where
		    hueb = mod' hue 1.0

		  

hls_to_rgb :: (Float,Float,Float) -> (Float,Float,Float)
hls_to_rgb (_,_,0.0) = (1.0,1.0,1.0) 
hls_to_rgb (h,l,s) = ((_v m1 m2 (h + (1.0/3.0))), (_v m1 m2 h), (_v m1 m2 (h-(1.0/3.0))))
	   where
		m1 = (2.0 * l) - m2 
		m2 = if (l <= 0.5) then (l * (1.0+s)) else (l + s -(l*s))

hsla2color :: (Float,Float,Float,Float) -> Color
hsla2color (h,s,l,a) = Color r g b a
	where
	     (r,g,b) = hls_to_rgb (hb,l,s)
	     hb = if (h > 1.0) then (h - 1.0) else 
	     	      if (h < 0.0) then (h + 1.0) else h

