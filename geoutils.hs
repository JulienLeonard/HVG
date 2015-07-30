module Geoutils where

-- Geometry types

data Point  = Point Float Float deriving (Show)

data Vector = Vector Float Float deriving (Show)

data Circle = Circle Point Float deriving (Show)

-- Geometry utils

--- length of a vector
vdist :: Vector -> Float
vdist (Vector x y) = sqrt (x*x + y*y)

---  add a point and a vector
padd (Point x1 y1) (Vector x2 y2) = Point (x1+x2) (y1+y2)

---  normalized a vector
vnorm :: Vector -> Vector
vnorm (Vector 0.0 0.0) = Vector 0.0 0.0
vnorm (Vector x y) 
      = Vector (x/vecdist) (y/vecdist)
      where
         vecdist = vdist(Vector x y)

--- build vector from 2 points
vector :: Point -> Point -> Vector
vector (Point x1 y1) (Point x2 y2) = Vector (x2-x1) (y2-y1)

--- rotate a vector
vrotate :: Vector -> Float -> Vector
vrotate (Vector x1 y1) a 
	= Vector (x1 * cosa - y1 * sina) (x1 * sina + y1 * cosa )
	where
	   cosa = (cos a)
	   sina = (sin a)

-- Circle utils

--- get the radius of a circle
cradius :: Circle -> Float
cradius (Circle _ r) = r

--- get the center of a circle
ccenter :: Circle -> Point
ccenter (Circle center _) = center

--- check if 2 circles intersects
cintersects :: Circle -> Circle -> Bool
cintersects (Circle c1 r1) (Circle c2 r2) = vdist(vector c1 c2) < (r1 + r2)

--- adj circle given radius and angle
adjcircle :: Circle -> Float -> Float -> Circle
adjcircle (Circle c1 r1) r a = Circle (padd c1 (vrotate (Vector 0.0 (r1+r)) a)) r

