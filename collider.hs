module Collider where

import Geoutils
import CircleNode

data Collider a = Collider {collidernodes :: [CircleNode a]}

collidercircles :: Collider a -> [Circle]
collidercircles = nodescircles . collidernodes

isnodecolliding :: Collider a -> CircleNode a -> Bool
isnodecolliding collider newnode = iscolliding (collidercircles collider) (nodecircle newnode)

collider_expand :: Collider a -> [CircleNode a] -> Collider a
collider_expand (Collider oldnodes) newnodes = Collider (oldnodes ++ newnodes)

