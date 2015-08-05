module CirclePacking where

import Geoutils

data Seed = Seed (Circle,Circle) Float deriving (Show)

--- seed factory
circlepairs2seeds :: [(Circle,Circle)] -> [Float] -> [Seed]
circlepairs2seeds pairs sides = [Seed pair side | pair <- pairs, side <- sides]

circlepair0 = ((Circle (Point 0.0 0.0) 1.0),(Circle (Point 2.0 0.0) 1.0))

allsides = [-1.0,1.0]

seeds0 = circlepairs2seeds [circlepair0] allsides

--- adj circle given radius and angle
--- TODO: add edge cases (cosv not in [-1.0,1.0] or denom == 0.0)
circles2circle :: (Circle,Circle) -> Float -> Float -> Circle
circles2circle ((Circle c1 r1),(Circle c2 r2)) radius side = Circle newcenter radius
	       where
	            newcenter = padd c2 vnew
		    vnew = vscale (vnorm (vrotate s3 angle) ) l2
		    s3  = vector c2 c1
		    l3  = vdist s3
		    l1  = r1 + radius
		    l2  = r2 + radius
		    denom = 2.0 * l2 * l3
		    cosv  = (l3 * l3 - l1 * l1 + l2 * l2) / denom
		    angle = acos( cosv ) * side


--- simple packing
circlepacking :: [Seed] -> Integer -> [Circle]
circlepacking [] _ = []
circlepacking _  0 = []
circlepacking ((Seed (c0,c1) side):xseeds) niter = [newc] ++ (circlepacking (xseeds ++ newseeds) (niter - 1))
	      where
	      	  newseeds = circlepairs2seeds [(c0,newc),(c1,newc)] allsides
	          newc     = circles2circle (c0,c1) (cradius c0) side


