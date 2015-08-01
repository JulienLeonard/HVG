module CirclePattern where

import Geoutils

--- adj circle given radius and angle
nextadjcircle :: Circle -> (Float,Float) -> Circle
nextadjcircle (Circle c1 r1) (r,a) = Circle (padd c1 (vrotate (Vector (r1+r) 0.0) a)) r

--- generate a list of circles from a list of radius
circlestring :: Circle -> [(Float,Float)] -> [Circle]
circlestring _ [] = []
circlestring c (ra:ras) = [c] ++ (circlestring (nextadjcircle c ra) ras)

