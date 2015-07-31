module Mathutils where

data Range  = Range Float Float deriving (Show)

rangeangle = Range 0.0 (2.0 * pi)

sample :: Range -> Float -> Float
sample (Range v1 v2) x = v1 + (v2 - v1) * x

samples :: Range -> Integer -> [Float] 
samples r nsamples = [(sample r  ((fromIntegral i)/(fromIntegral  (nsamples - 1)))) | i <- [0..nsamples-1]]

geo :: Float -> Float -> Integer -> [Float]
geo root ratio 0 = []
geo root ratio nvalues = [root] ++ (geo (root*ratio) ratio (nvalues-1))