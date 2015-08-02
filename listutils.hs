module Listutils where

lzip2 :: [[a]] -> [(a,a)]
lzip2 [a,b] = zip a b