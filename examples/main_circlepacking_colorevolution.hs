import Geoutils
import Color
import Render
import Mathutils
import Listutils
import DrawUtils
import CircleNode
import CirclePacking
import Collider
import System.Random

nodehue = nodecontent

fnewcontent :: [CircleNode Float] -> (CirclePackingContext [Float]) -> Float
fnewcontent nodes context =  maxhue + (sample (symRange 0.1) ((take (newindex+1) (contextcontent context)) !! newindex))
	    where 
	        maxhue   = maximum $ map nodehue nodes
		newindex = contextniter context

fnewcontext context = context

main = do
     writeFile "circlepacking_colorand.svg" $ svgCircleColors circlecolors
     where 
         circlecolors  = [((nodecircle node),(hue2color (nodehue node))) | node <- newnodes]
	 newnodes      = seed0nodes ++ (circlepacking (Collider seed0nodes) hueseeds0 context0 circlepackingspec 0.1 niter)
	 fnewradius    = fratioNewRadius 0.9
	 seed0nodes    = circlenodesfromseeds hueseeds0
	 hueseeds0     = seeds00 0.0 0.0
	 context0      = context00 randomgen
	 randomgen     = randomRs (0.0,1.0) (mkStdGen 0) :: [Float]
	 niter         = 5000
	 circlepackingspec = CirclePackingSpec fnewradius fnewcontent fnewcontext fappendnewseeds
	 
