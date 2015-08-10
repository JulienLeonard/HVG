module CircleNode where

import Geoutils

--- CircleNode definition
data CircleNode nodeContent = CircleNode {nodecircle :: Circle,
			        nodecontent :: nodeContent,
			        nodeprevs   :: [CircleNode nodeContent]} deriving (Show)

circle2circlenode :: Circle -> nodeContent -> CircleNode nodeContent
circle2circlenode c a = CircleNode c a []

nodescircles :: [CircleNode nodeContent] -> [Circle]
nodescircles nodes = [nodecircle node | node <- nodes]
