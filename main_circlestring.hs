import Geoutils
import Color
import Render
import CirclePattern
import Mathutils
import Listutils
import DrawUtils

--- compute a circlestring with an angle offset
patterncircles :: Circle -> Float -> [Circle]
patterncircles c offset = circlestring c ras
	     where
	     	ras = map (\x -> ((fst x),((snd x) + offset))) (lzip2 (map ($ 100) [(geo 100.0 0.9),(samples rangeangle)]))	       

main = do
     writeFile "redcircles.svg" $ svgCircles circles red
     where 
         circles = (concat [(patterncircles c0 offset) | offset <- (samples rangeangle 5)])
