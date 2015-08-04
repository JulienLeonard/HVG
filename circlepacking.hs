module CirclePacking where

import Geoutils

--- adj circle given radius and angle
--- TODO: add edge cases (cosv not in [-1.0,1.0] or denom == 0.0)
circles2circle :: Circle -> Circle -> Float -> Float -> Circle
circles2circle (Circle c1 r1) (Circle c2 r2) radius side = Circle newcenter radius
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



