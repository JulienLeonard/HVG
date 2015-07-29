-- Geometry types

data Point  = Point Float Float deriving (Show)

data Vector = Vector Float Float deriving (Show)

data Circle = Circle Point Float deriving (Show)

-- Geometry utils

padd (Point x1 y1) (Vector x2 y2) = Point (x1+x2) (y1+y2)


-- tests

test_padd = padd (Point 1.0 2.0) (Vector 3.0 4.0)

