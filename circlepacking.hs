module CirclePacking where

import Geoutils
import CircleNode
import Collider

type Niter = Int

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

circlenodepair0 content = CircleNodePair (circle2circlenode (Circle (Point 0.0 0.0) 1.0) content) (circle2circlenode (Circle (Point 2.0 0.0) 1.0) content)
circlenodepair00 content1 content2 = CircleNodePair (circle2circlenode (Circle (Point 0.0 0.0) 1.0) content1) (circle2circlenode (Circle (Point 2.0 0.0) 1.0) content2)


allsides = [SideLeft,SideRight]

seeds0 content = circlenodepairs2seeds [circlenodepair0 content] allsides
seeds00 content1 content2 = circlenodepairs2seeds [circlenodepair00 content1 content2] allsides

circlesfromseeds :: [Seed a] -> [Circle]
circlesfromseeds [] = []
circlesfromseeds (seed:seeds) = (seedcircles seed) ++ (circlesfromseeds seeds)

circlenodesfromseeds :: [Seed a] -> [CircleNode a]
circlenodesfromseeds [] = []
circlenodesfromseeds (seed:seeds) = (seednodes seed) ++ (circlenodesfromseeds seeds)


type RatioRadius = Float


---- CirclePackingContext
---- Give global computation context to node creation, instead of parent ones

data CirclePackingContext a = CirclePackingContext {contextniter   :: Niter,
                                                    contextcontent :: a}

context00 a = CirclePackingContext 0 a

--- generators methods
type FNodeNewContent a b =  ([CircleNode a] -> (CirclePackingContext b) -> a)
type FNodeNewRadius  a b =  ([CircleNode a] -> (CirclePackingContext b) -> Radius)

fratioNewRadius :: RatioRadius -> [CircleNode a] -> (CirclePackingContext b) -> Radius
fratioNewRadius ratio parentnodes _ = r0 * ratio
	       where
			r0 = noderadius $ parentnodes !! 0

--- 
seed2circlenodes :: Seed a -> CirclePackingContext b -> (FNodeNewRadius a b) -> (FNodeNewContent a b) -> CircleNode a
seed2circlenodes (Seed (CircleNodePair node1 node2) side) context fnewradius fnewnodecontent = newnode
		 where
		        newnode     = CircleNode newc newcontent parentnodes
			newc        = (circles2circle (nodecircle node1) (nodecircle node2) side newradius)
			newradius   = fnewradius      parentnodes context
			newcontent  = fnewnodecontent parentnodes context
			parentnodes = [node1,node2]
			
--- trimcollidings: 
trimcollidings :: Collider a -> [CircleNode a] -> [CircleNode a]
trimcollidings _ [] = []
trimcollidings collider (cnode:nodes) = newnodes ++ (trimcollidings newcollider nodes)
	       where 
	           newcollider = collider_expand collider newnodes
		   newnodes    = if (isnodecolliding collider cnode) then [] else [cnode]

---
type FNewContext b = (CirclePackingContext b -> CirclePackingContext b)
type FNewSeeds a   = [Seed a] -> [Seed a] -> [Seed a]

fappendnewseeds :: FNewSeeds a
fappendnewseeds oldseeds newseeds = oldseeds ++ newseeds

--- simple packing
circlepacking :: Collider a -> [Seed a] -> CirclePackingContext b -> (FNodeNewRadius a b) -> (FNodeNewContent a b) -> (FNewContext b) -> (FNewSeeds a) -> Niter -> [CircleNode a]
circlepacking _ [] _ _ _ _ _ _ = []
circlepacking _ _  _ _ _ _ _ 0 = []
circlepacking collider (cseed:xseeds) context fnewradius fnewcontent fnewcontext fnewseeds niter = newnodes ++ (circlepacking newcollider newseeds newcontext fnewradius fnewcontent fnewcontext fnewseeds (niter - 1))
	      where
	          newcollider  = collider_expand collider newnodes
		  newcontext   = fnewcontext (CirclePackingContext ((contextniter context) + 1) (contextcontent context))
		  newseeds     = fnewseeds xseeds createdseeds
	      	  createdseeds = circlenodepairs2seeds (concat [ [(CircleNodePair n1 newnode),(CircleNodePair n2 newnode)] | newnode <- newnodes]) allsides
	          newnodes     = trimcollidings collider [(seed2circlenodes cseed context fnewradius fnewcontent)]
		  newradius    = fnewradius  
		  n1           = cnode1 (seednodepair cseed)
		  n2           = cnode2 (seednodepair cseed)
		  side         = seedside cseed


