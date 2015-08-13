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

fnewcontent :: [CircleNode Float] -> (CirclePackingContext ([Float],[Float])) -> Float
fnewcontent nodes context = newhue 
	    where 
	        newhue   = maxhue + (randhues !! newindex)
		randhues = fst (contextcontent context)
	        maxhue   = maximum $ map nodehue nodes
		newindex = contextniter context

fnewcontext context = context

fnewradius :: [CircleNode Float] -> (CirclePackingContext ([Float],[Float])) -> Float
fnewradius nodes context =  newradius
	    where 
	          newradius  = maxradius * (1.0 + (sample (Range (-0.1) 0.2) (randratios !! newindex)))
		  randratios = snd (contextcontent context)
	    	  maxradius  = maximum $ map noderadius nodes
		  newindex   = contextniter context

main = do
     writeFile "circlepackingradius.svg" $ svgCircleColors circlecolors
     where 
         circlecolors     = [((nodecircle node),(hue2color (nodehue node))) | node <- newnodes]
	 newnodes         = seed0nodes ++ (circlepacking (Collider seed0nodes) hueseeds0 context0 fnewradius fnewcontent fnewcontext fappendnewseeds niter)
	 seed0nodes       = circlenodesfromseeds hueseeds0
	 hueseeds0        = seeds00 0.0 0.0
	 context0         = context00 (randincrhues,randratioradius)
	 randratioradius  = take niter $ randomRs (-0.1,0.1) (mkStdGen 0) :: [Float]
	 randincrhues     = take niter $ randomRs (-0.1,0.1) (mkStdGen 1) :: [Float]
	 niter            = 5000
	 
