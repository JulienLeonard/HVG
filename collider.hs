module Collider where

import Geoutils
import CircleNode

data Collider = Collider {collidernodes :: [CircleNode]}

collidercircles :: Collider -> [Circle]
collidercircles = nodescircles . collidernodes

isnodecolliding :: Collider -> CircleNode -> Bool
isnodecolliding collider newnode = iscolliding (collidercircles collider) (nodecircle newnode)

collider_expand :: Collider -> [CircleNode] -> Collider
collider_expand (Collider oldnodes) newnodes = Collider (oldnodes ++ newnodes)

