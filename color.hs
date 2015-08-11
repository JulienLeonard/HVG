module Color where

import Data.Fixed

type R = Float
type G = Float
type B = Float
type A = Float

data RGBA = RGBA R G B A deriving (Show)

type Hue = Float
type Saturation = Float
type Light = Float


data HSLA = HSLA Hue Saturation Light A deriving (Show)

white = (RGBA 1.0 1.0 1.0 1.0)
red   = (RGBA 1.0 0.0 0.0 1.0)
black = (RGBA 0.0 0.0 0.0 1.0)

coloropacity :: RGBA -> Float
coloropacity (RGBA _ _ _ a) = a

float2int255 :: Float -> Integer
float2int255 v = round (255.0 * v)

rgba2svg :: RGBA -> String
rgba2svg (RGBA r g b _) =  (show ir)++","++(show ig)++","++(show ib)
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

hsla2rgba :: HSLA -> RGBA
hsla2rgba (HSLA h s l a) = RGBA r g b a
	where
	     (r,g,b) = hls_to_rgb (hb,l,s)
	     hb = if (h > 1.0) then (h - 1.0) else 
	     	      if (h < 0.0) then (h + 1.0) else h

