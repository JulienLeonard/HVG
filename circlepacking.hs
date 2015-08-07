module CirclePacking where

import Geoutils

data CirclePair = CirclePair {cpc1 :: Circle,
                              cpc2 ::  Circle} deriving (Show)

data Side  = SideLeft | SideRight deriving (Enum,Show,Eq)

side2float :: Side -> Float
side2float side = if side == SideLeft then -1.0 else 1.0 

data Seed = Seed {seedcirclepair :: CirclePair,
                  seedside :: Side} deriving (Show)

--- seed factory
circlepairs2seeds :: [CirclePair] -> [Side] -> [Seed]
circlepairs2seeds pairs sides = concat [ [Seed pair side | pair <- pairs] | side <- sides ]

circlepair0 = CirclePair (Circle (Point 0.0 0.0) (Radius 1.0)) (Circle (Point 2.0 0.0) (Radius 1.0))

allsides = [SideLeft,SideRight]

seeds0 = circlepairs2seeds [circlepair0] allsides

circlesfromseeds :: [Seed] -> [Circle]
circlesfromseeds [] = []
circlesfromseeds ((Seed (CirclePair c1 c2) _ ):seeds) = [c1,c2] ++ (circlesfromseeds seeds)


--- adj circle given radius and angle
--- TODO: add edge cases (cosv not in [-1.0,1.0] or denom == 0.0)
circles2circle :: Seed -> Radius -> Circle
circles2circle (Seed (CirclePair (Circle c1 r1) (Circle c2 r2)) side) radius = Circle newcenter radius
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

--- circlecollidings:
circlecollidings :: [Circle] -> Circle -> Bool
circlecollidings [] _ = False
circlecollidings (c:cs) newc = (cintersects c newc) || (circlecollidings cs newc)

--- trimcollidings: 
trimcollidings :: [Circle] -> [Circle] -> [Circle]
trimcollidings _ [] = []
trimcollidings crefs (c:cs) = newcs ++ (trimcollidings (newcs ++ crefs) cs)
	       where 
	           newcs = if (circlecollidings crefs c) then [] else [c]

--- simple packing
circlepacking :: [Circle] -> [Seed] -> Float -> Integer -> [Circle]
circlepacking _ [] _ _ = []
circlepacking _ _ _ 0 = []
circlepacking cs ((Seed (CirclePair c0 c1) side):xseeds) ratio niter = newcs ++ (circlepacking (cs ++ newcs) (xseeds ++ newseeds) ratio (niter - 1))
	      where
	      	  newseeds = circlepairs2seeds (concat [ [(CirclePair c0 newc),(CirclePair c1 newc)] | newc <- newcs]) allsides
	          newcs    = trimcollidings cs [circles2circle (Seed (CirclePair c0 c1) side) (Radius ( ( radius2float (cradius c0) ) * ratio) )] 


