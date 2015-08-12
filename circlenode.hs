module CircleNode where

import Geoutils

--- CircleNode definition
data CircleNode a = CircleNode {nodecircle  :: Circle,
			        nodecontent :: a,
			        nodeseed    :: [CircleNode a]} deriving (Show)

noderadius = cradius . nodecircle

circle2circlenode :: Circle -> a -> CircleNode a
circle2circlenode c content = CircleNode c content []

nodescircles :: [CircleNode a] -> [Circle]
nodescircles nodes = map nodecircle nodes


