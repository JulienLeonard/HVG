module CirclePacking where

import Geoutils
import CircleNode
import Collider


--- Seed definition
data CircleNodePair a = CircleNodePair {cnode1 :: CircleNode a,
     		               	        cnode2 :: CircleNode a} deriving (Show)

nodepairnodes :: CircleNodePair a -> [CircleNode a]
nodepairnodes (CircleNodePair node1 node2) = [node1,node2]

nodepaircircles = nodescircles . nodepairnodes

data Seed a = Seed {seednodepair :: CircleNodePair a,
                    seedside :: Side} deriving (Show)

seedcircles :: Seed a -> [Circle]
seedcircles = nodepaircircles . seednodepair

seednodes :: Seed a -> [CircleNode a]
seednodes = nodepairnodes . seednodepair

--- seed factory
circlenodepairs2seeds :: [CircleNodePair a] -> [Side] -> [Seed a]
circlenodepairs2seeds pairs sides = concat [ [Seed pair side | pair <- pairs] | side <- sides ]

circlenodepair0 content = CircleNodePair (circle2circlenode (Circle (Point 0.0 0.0) (Radius 1.0)) content) (circle2circlenode (Circle (Point 2.0 0.0) (Radius 1.0)) content)
circlenodepair00 content1 content2 = CircleNodePair (circle2circlenode (Circle (Point 0.0 0.0) (Radius 1.0)) content1) (circle2circlenode (Circle (Point 2.0 0.0) (Radius 1.0)) content2)


allsides = [SideLeft,SideRight]

seeds0 content = circlenodepairs2seeds [circlenodepair0 content] allsides
seeds00 content1 content2 = circlenodepairs2seeds [circlenodepair00 content1 content2] allsides

circlesfromseeds :: [Seed a] -> [Circle]
circlesfromseeds [] = []
circlesfromseeds (seed:seeds) = (seedcircles seed) ++ (circlesfromseeds seeds)

circlenodesfromseeds :: [Seed a] -> [CircleNode a]
circlenodesfromseeds [] = []
circlenodesfromseeds (seed:seeds) = (seednodes seed) ++ (circlenodesfromseeds seeds)


data RatioRadius = RatioRadius Float

ratioradius2float :: RatioRadius -> Float
ratioradius2float (RatioRadius ratio) = ratio

--- 
seed2circlenodes :: Seed a -> Radius -> ([CircleNode a] -> a) -> CircleNode a
seed2circlenodes (Seed (CircleNodePair (CircleNode c1 rank1 prevs1) (CircleNode c2 rank2 prevs2)) side) radius fnewnodecontent = CircleNode newc (fnewnodecontent parentnodes) parentnodes
		 where
			newc        = (circles2circle c1 c2 side radius)
			parentnodes = [(CircleNode c1 rank1 prevs1),(CircleNode c2 rank2 prevs2)]
			
--- trimcollidings: 
trimcollidings :: Collider a -> [CircleNode a] -> [CircleNode a]
trimcollidings _ [] = []
trimcollidings collider (cnode:nodes) = newnodes ++ (trimcollidings newcollider nodes)
	       where 
	           newcollider = collider_expand collider newnodes
		   newnodes    = if (isnodecolliding collider cnode) then [] else [cnode]

--- simple packing
circlepacking :: Collider a -> [Seed a] -> RatioRadius -> ([CircleNode a] -> a) -> Integer -> [CircleNode a]
circlepacking _ [] _ _ _ = []
circlepacking _ _  _ _ 0 = []
circlepacking collider (cseed:xseeds) ratio fnewcontent niter = newnodes ++ (circlepacking newcollider newseeds ratio fnewcontent (niter - 1))
	      where
	          newcollider  = collider_expand collider newnodes
		  newseeds     = (xseeds ++ createdseeds)
	      	  createdseeds = circlenodepairs2seeds (concat [ [(CircleNodePair n0 newnode),(CircleNodePair n1 newnode)] | newnode <- newnodes]) allsides
	          newnodes     = trimcollidings collider [(seed2circlenodes cseed newradius fnewcontent)]
		  newradius    = Radius (( radius2float r0) * (ratioradius2float ratio))
		  r0           = cradius (nodecircle n0)
		  n0           = cnode1 (seednodepair cseed)
		  n1           = cnode2 (seednodepair cseed)
		  side         = seedside cseed


