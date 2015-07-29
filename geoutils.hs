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
vnorm (Vector x y) = Vector (x/vdist(Vector x y)) (y/vdist(Vector x y))

--- build vector from 2 points
vector :: Point -> Point -> Vector
vector (Point x1 y1) (Point x2 y2) = Vector (x2-x1) (y2-y1)

-- Circle utils

cradius :: Circle -> Float
cradius (Circle _ r) = r

ccenter :: Circle -> Point
ccenter (Circle center _) = center

cintersects :: Circle -> Circle -> Bool
cintersects (Circle c1 r1) (Circle c2 r2) = vdist(vector c1 c2) < (r1 + r2)

-- tests

test_padd = padd (Point 1.0 2.0) (Vector 3.0 4.0)
test_vnorm = vnorm (Vector 1.0 1.0)
test_cradius = cradius (Circle (Point 0.0 0.0) 1.5)
test_ccenter = ccenter (Circle (Point 0.0 0.1) 1.5)
test_vector = vector (Point 1.0 2.0) (Point 3.0 5.0)
test_cintersects = cintersects (Circle (Point 0.0 0.0) 1.0) (Circle (Point 2.0 2.0) 1.0)