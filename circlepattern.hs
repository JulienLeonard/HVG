module CirclePattern where

import Geoutils

--- adj circle given radius and angle
nextadjcircle :: Circle -> (Radius,Angle) -> Circle
nextadjcircle (Circle c1 (Radius r1)) ((Radius r),a) = Circle (padd c1 (vrotate (Vector (r1+r) 0.0) a)) (Radius r)

--- generate a list of circles from a list of radius
circlestring :: Circle -> [(Radius,Angle)] -> [Circle]
circlestring _ [] = []
circlestring c (ra:ras) = [c] ++ (circlestring (nextadjcircle c ra) ras)

