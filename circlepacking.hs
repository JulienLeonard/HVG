module CirclePacking where

import Geoutils
import CircleNode
import Collider


--- Seed definition
data CircleNodePair = CircleNodePair {cnode1 :: CircleNode,
     		                      cnode2 :: CircleNode} deriving (Show)

nodepairnodes :: CircleNodePair -> [CircleNode]
nodepairnodes (CircleNodePair node1 node2) = [node1,node2]

nodepaircircles = nodescircles . nodepairnodes

data Seed = Seed {seednodepair :: CircleNodePair,
                  seedside :: Side} deriving (Show)

seedcircles :: Seed -> [Circle]
seedcircles = nodepaircircles . seednodepair

seednodes :: Seed -> [CircleNode]
seednodes = nodepairnodes . seednodepair

--- seed factory
circlenodepairs2seeds :: [CircleNodePair] -> [Side] -> [Seed]
circlenodepairs2seeds pairs sides = concat [ [Seed pair side | pair <- pairs] | side <- sides ]

circlenodepair0 = CircleNodePair (circle2circlenode (Circle (Point 0.0 0.0) (Radius 1.0))) (circle2circlenode (Circle (Point 2.0 0.0) (Radius 1.0)))

allsides = [SideLeft,SideRight]

seeds0 = circlenodepairs2seeds [circlenodepair0] allsides

circlesfromseeds :: [Seed] -> [Circle]
circlesfromseeds [] = []
circlesfromseeds (seed:seeds) = (seedcircles seed) ++ (circlesfromseeds seeds)

circlenodesfromseeds :: [Seed] -> [CircleNode]
circlenodesfromseeds [] = []
circlenodesfromseeds (seed:seeds) = (seednodes seed) ++ (circlenodesfromseeds seeds)


data RatioRadius = RatioRadius Float

ratioradius2float :: RatioRadius -> Float
ratioradius2float (RatioRadius ratio) = ratio

--- 
seed2circlenodes :: Seed -> Radius -> CircleNode
seed2circlenodes (Seed (CircleNodePair (CircleNode c1 rank1 prevs1) (CircleNode c2 rank2 prevs2)) side) radius = CircleNode newc ( 1 + (max rank1 rank2)) parentnodes
		 where
			newc        = (circles2circle c1 c2 side radius)
			parentnodes = [(CircleNode c1 rank1 prevs1),(CircleNode c2 rank2 prevs2)]
			
--- trimcollidings: 
trimcollidings :: Collider -> [CircleNode] -> [CircleNode]
trimcollidings _ [] = []
trimcollidings collider (cnode:nodes) = newnodes ++ (trimcollidings newcollider nodes)
	       where 
	           newcollider = collider_expand collider newnodes
		   newnodes    = if (isnodecolliding collider cnode) then [] else [cnode]

--- simple packing
circlepacking :: Collider -> [Seed] -> RatioRadius -> Integer -> [CircleNode]
circlepacking _ [] _ _ = []
circlepacking _ _ _ 0 = []
circlepacking collider (cseed:xseeds) ratio niter = newnodes ++ (circlepacking newcollider newseeds ratio (niter - 1))
	      where
	          newcollider  = collider_expand collider newnodes
		  newseeds     = (xseeds ++ createdseeds)
	      	  createdseeds = circlenodepairs2seeds (concat [ [(CircleNodePair n0 newnode),(CircleNodePair n1 newnode)] | newnode <- newnodes]) allsides
	          newnodes     = trimcollidings collider [(seed2circlenodes cseed newradius)]
		  newradius    = Radius (( radius2float r0) * (ratioradius2float ratio))
		  r0           = cradius (nodecircle n0)
		  n0           = cnode1 (seednodepair cseed)
		  n1           = cnode2 (seednodepair cseed)
		  side         = seedside cseed


