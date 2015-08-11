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
fnewcontent nodes context =  maxhue + ((contextcontent context) !! newindex)
	    where 
	        maxhue   = maximum([nodehue node | node <- nodes])
		newindex = 1 + contextniter context

fnewcontext context = context

main = do
     writeFile "circlepacking_colorand.svg" $ svgCircleColors circlecolors
     where 
         circlecolors  = [((nodecircle node),(hsla2rgba (HSLA (nodehue node) 1.0 0.5 1.0))) | node <- newnodes]
	 newnodes      = seed0nodes ++ (circlepacking (Collider seed0nodes) hueseeds0 context0 (RatioRadius 0.9) fnewcontent fnewcontext niter)
	 seed0nodes    = circlenodesfromseeds hueseeds0
	 hueseeds0     = (seeds00 0.0 0.0)
	 context0      = context00 randincrhues
	 randincrhues  = take niter $ randomRs (-0.1,0.1) (mkStdGen 0) :: [Float]
	 niter         = 5000
	 
