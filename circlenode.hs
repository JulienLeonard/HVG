module CircleNode where

import Geoutils

--- CircleNode definition
data CircleNode = CircleNode {nodecircle :: Circle,
			      noderank   :: Integer,
			      nodeprevs  :: [CircleNode]} deriving (Show)

circle2circlenode :: Circle -> CircleNode
circle2circlenode c = CircleNode c 0 []

nodescircles :: [CircleNode] -> [Circle]
nodescircles nodes = [nodecircle node | node <- nodes]
