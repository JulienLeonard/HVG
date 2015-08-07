import Geoutils
import Color
import Render
import CirclePattern
import Mathutils
import Listutils
import DrawUtils

--- compute a circlestring with an angle offset
patterncircles :: Circle -> Angle -> [Circle]
patterncircles c offset = circlestring c ras
	     where
	     	ras = map (\x -> ((Radius (fst x)),(Angle ((snd x) + (angle2float offset))))) (lzip2 (map ($ 100) [(geo 100.0 0.9),(samples rangeangle)]))

main = do
     writeFile "redcircles.svg" $ svgCircles circles red
     where 
         circles = (concat [(patterncircles c0 (Angle offset)) | offset <- (samples rangeangle 5)])
