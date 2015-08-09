module Collider where

import Geoutils

data Collider = Collider {collidercircles :: [Circle]}

_iscolliding :: [Circle] -> Circle -> Bool
_iscolliding [] _ = False
_iscolliding (c:cs) newc = (cintersects c newc) || (_iscolliding cs newc)

iscolliding :: Collider -> Circle -> Bool
iscolliding (Collider circles) newc = _iscolliding circles newc


