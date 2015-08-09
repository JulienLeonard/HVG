module Geoutils where

-- Geometry types

data Point  = Point { px :: Float,
                      py :: Float } deriving (Show)

p0 = Point 0.0 0.0

data Vector = Vector { vx :: Float,
     	      	       vy :: Float } deriving (Show)

v0 = Vector 0.0 0.0
v0x = Vector 1.0 0.0
v0y = Vector 0.0 1.0

data Radius = Radius Float deriving (Show)

radius2float :: Radius -> Float
radius2float (Radius r) = r


data Angle = Angle Float deriving (Show)
angle2float :: Angle -> Float
angle2float (Angle a) = a

data Circle = Circle { ccenter :: Point,
     	      	       cradius :: Radius } deriving (Show)

c0 = Circle p0 (Radius 1.0)

data Side  = SideLeft | SideRight deriving (Enum,Show,Eq)

side2float :: Side -> Float
side2float side = if side == SideLeft then -1.0 else 1.0 


data Polygon = Polygon {ppoints :: [Point]} deriving (Show)

-- in the form of (Point xmin ymin) (Point xmax ymax)
data BBox = BBox { bbpmin :: Point,
                   bbpmax :: Point } deriving (Show)

-- Geometry utils

--- middle of 2 points
pmiddle :: Point -> Point -> Point
pmiddle (Point x1 y1) (Point x2 y2) = Point ((x1+x2)/2.0) ((y1+y2)/2.0) 

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
vrotate :: Vector -> Angle -> Vector
vrotate (Vector x1 y1) (Angle a) 
	= Vector (x1 * cosa - y1 * sina) (x1 * sina + y1 * cosa )
	where
	   cosa = (cos a)
	   sina = (sin a)

--- scale a vector
vscale :: Vector -> Float -> Vector
vscale (Vector x y) ratio
        = Vector (x * ratio) (y * ratio)


-- Circle utils

--- check if 2 circles intersects
cintersects :: Circle -> Circle -> Bool
cintersects (Circle c1 (Radius r1)) (Circle c2 (Radius r2)) = vdist(vector c1 c2) - (r1 + r2) < (-0.0001 * (r1 + r1))

iscolliding :: [Circle] -> Circle -> Bool
iscolliding [] _ = False
iscolliding (c:cs) newc = (cintersects c newc) || (iscolliding cs newc)

--- compute a list of points from the circle
circlePolygon :: Circle -> Polygon
circlePolygon (Circle (Point x1 y1) (Radius r1)) = 
	 Polygon [padd (Point x1 y1) (vrotate (Vector 0.0 r1) (Angle ((pi * 2.0) * (i / 100)))) | i <- [0..99]]


--- adj circle given radius and angle
--- TODO: add edge cases (cosv not in [-1.0,1.0] or denom == 0.0)
circles2circle :: Circle -> Circle -> Side -> Radius -> Circle
circles2circle (Circle c1 r1) (Circle c2 r2) side radius = Circle newcenter radius
	       where
	            newcenter = padd c2 vnew
		    vnew = vscale (vnorm (vrotate s3 angle) ) l2
		    s3  = vector c2 c1
		    l3  = vdist s3
		    l1  = (radius2float r1) + (radius2float radius)
		    l2  = (radius2float r2) + (radius2float radius)
		    denom = 2.0 * l2 * l3
		    cosv  = (l3 * l3 - l1 * l1 + l2 * l2) / denom
		    angle = Angle (acos( cosv ) * (side2float side))


--- bounding box
bboxPolygon :: Polygon -> BBox
bboxPolygon (Polygon [pend]) = BBox pend pend
bboxPolygon (Polygon (p:ps)) = maxbb p (bboxPolygon (Polygon ps))

maxbb :: Point -> BBox -> BBox
maxbb (Point newx newy) (BBox (Point xmin ymin) (Point xmax ymax)) = BBox newpmin newpmax 
      where
	newpmin = Point (min newx xmin) (min newy ymin) 
	newpmax = Point (max newx xmax) (max newy ymax) 

mergeBBox :: BBox -> BBox -> BBox
mergeBBox (BBox (Point xmin1 ymin1) (Point xmax1 ymax1)) (BBox (Point xmin2 ymin2) (Point xmax2 ymax2)) = BBox (Point (min xmin1 xmin2) (min ymin1 ymin2)) (Point (max xmax1 xmax2) (max ymax1 ymax2))

bboxPolygons :: [Polygon] -> BBox
bboxPolygons [poly] = bboxPolygon poly
bboxPolygons (poly:polys) = mergeBBox (bboxPolygon poly) (bboxPolygons polys)

