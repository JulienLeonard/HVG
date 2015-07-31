module CirclePattern where

import Geoutils

--- adj circle given radius and angle
nextadjcircle :: Circle -> (Float,Float) -> Circle
nextadjcircle (Circle c1 r1) (r,a) = Circle (padd c1 (vrotate (Vector (r1+r) 0.0) a)) r

--- generate a list of circles from a list of radius
--- TODO: refactor
circlestring :: [Circle] -> Circle -> [(Float,Float)] -> [Circle]
circlestring _ c [] = [c]
circlestring cs c ((r,a):ras)  = cs ++ [c] ++ (circlestring [] (nextadjcircle c (r,a)) ras)

