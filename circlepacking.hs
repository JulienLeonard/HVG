module CirclePacking where

import Geoutils

--- CircleNode definition
data CircleNode = CircleNode {nodecircle :: Circle,
			      noderank   :: Integer,
			      nodeprevs  :: [CircleNode]} deriving (Show)

circle2circlenode :: Circle -> CircleNode
circle2circlenode c = CircleNode c 0 []

nodecircles :: [CircleNode] -> [Circle]
nodecircles nodes = [nodecircle node | node <- nodes]

--- Seed definition
data CircleNodePair = CircleNodePair {cnode1 :: CircleNode,
     		                      cnode2 :: CircleNode} deriving (Show)

nodepairnodes :: CircleNodePair -> [CircleNode]
nodepairnodes (CircleNodePair node1 node2) = [node1,node2]

data Side  = SideLeft | SideRight deriving (Enum,Show,Eq)

side2float :: Side -> Float
side2float side = if side == SideLeft then -1.0 else 1.0 

data Seed = Seed {seednodepair :: CircleNodePair,
                  seedside :: Side} deriving (Show)

--- seed factory
circlenodepairs2seeds :: [CircleNodePair] -> [Side] -> [Seed]
circlenodepairs2seeds pairs sides = concat [ [Seed pair side | pair <- pairs] | side <- sides ]

circlenodepair0 = CircleNodePair (circle2circlenode (Circle (Point 0.0 0.0) (Radius 1.0))) (circle2circlenode (Circle (Point 2.0 0.0) (Radius 1.0)))

allsides = [SideLeft,SideRight]

seeds0 = circlenodepairs2seeds [circlenodepair0] allsides

circlesfromseeds :: [Seed] -> [Circle]
circlesfromseeds [] = []
circlesfromseeds ((Seed (CircleNodePair (CircleNode c1 _ _) (CircleNode c2 _ _)) _):seeds) = [c1,c2] ++ (circlesfromseeds seeds)



data RatioRadius = RatioRadius Float

ratioradius2float :: RatioRadius -> Float
ratioradius2float (RatioRadius ratio) = ratio


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

--- 
seed2circlenodes :: Seed -> Radius -> CircleNode
seed2circlenodes (Seed (CircleNodePair (CircleNode c1 rank1 prevs1) (CircleNode c2 rank2 prevs2)) side) radius = CircleNode newc ( 1 + (max rank1 rank2)) parentnodes
		 where
			newc        = (circles2circle c1 c2 side radius)
			parentnodes = [(CircleNode c1 rank1 prevs1),(CircleNode c2 rank2 prevs2)]
			

--- circlecollidings:
circlecollidings :: [Circle] -> Circle -> Bool
circlecollidings [] _ = False
circlecollidings (c:cs) newc = (cintersects c newc) || (circlecollidings cs newc)

--- trimcollidings: 
trimcollidings :: [Circle] -> [CircleNode] -> [CircleNode]
trimcollidings _ [] = []
trimcollidings crefs (cnode:nodes) = newnodes ++ (trimcollidings (newcs ++ crefs) nodes)
	       where 
	           newcs    = [nodecircle newnode | newnode <- newnodes]  
		   newnodes = if (circlecollidings crefs (nodecircle cnode)) then [] else [cnode]

--- simple packing
circlepacking :: [Circle] -> [Seed] -> RatioRadius -> Integer -> [CircleNode]
circlepacking _ [] _ _ = []
circlepacking _ _ _ 0 = []
circlepacking colliders ((Seed (CircleNodePair n0 n1) side):xseeds) ratio niter = newnodes ++ (circlepacking newcolliders newseeds ratio (niter - 1))
	      where
	          newcolliders = (colliders ++ newcs)
		  newseeds     = (xseeds ++ createdseeds)
	      	  createdseeds = circlenodepairs2seeds (concat [ [(CircleNodePair n0 newnode),(CircleNodePair n1 newnode)] | newnode <- newnodes]) allsides
		  newcs        = [nodecircle node | node <- newnodes]
	          newnodes     = trimcollidings colliders [(seed2circlenodes cseed newradius)]
		  newradius    = Radius (( radius2float (cradius (nodecircle n0))) * (ratioradius2float ratio))
		  cseed        = (Seed (CircleNodePair n0 n1) side) 


