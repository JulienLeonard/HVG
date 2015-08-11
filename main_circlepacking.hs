import Geoutils
import Color
import Render
import Mathutils
import Listutils
import DrawUtils
import CircleNode
import CirclePacking
import Collider

-- Specialised CircleNode with a rank content
type Rank =  Integer
rank0 = 0

noderank = nodecontent

fnewrank :: [CircleNode Rank] -> (CirclePackingContext Dummy) -> Rank
fnewrank nodes context =  1 + (maximum [(noderank node) | node <- nodes])

nodehue :: Rank -> CircleNode Rank -> Hue
nodehue maxrank node = ((fromIntegral (noderank node))/(fromIntegral maxrank))


-- Specialised CirclePackingContext with nothin
fnewcontext context = context
type Dummy = Integer
dummy0 = 0

--- Main computation
main = do
     writeFile "redcirclepacking.svg" $ svgCircleColors circlecolors
     where 
         circlecolors = [((nodecircle node),(hsla2rgba (HSLA hue 1.0 0.5 1.0)))  | (node,hue) <- (zip newnodes newhues)]
	 newhues      = map (nodehue maxrank) newnodes
	 maxrank      = maximum $ map noderank newnodes
	 newnodes     = seed0nodes ++ (circlepacking (Collider seed0nodes) rankseeds0 context0 (RatioRadius 0.9) fnewrank fnewcontext 1000)
	 context0     = context00 dummy0
	 seed0nodes   = circlenodesfromseeds rankseeds0
	 rankseeds0   = seeds0 rank0
