module Test_Geoutils where

import Geoutils
import Test.HUnit

test_padd  = TestCase (assertEqual "test padd"  (Point 4.0 6.0) (padd (Point 1.0 2.0) (Vector 3.0 4.0)))
test_vnorm = TestCase (assertEqual "test vnorm" (Vector 0.0 1.0)  (vnorm (Vector 0.0 2.0)))
--test_cradius = cradius (Circle (Point 0.0 0.0) 1.5)
--test_ccenter = ccenter (Circle (Point 0.0 0.1) 1.5)
--test_vector = vector (Point 1.0 2.0) (Point 3.0 5.0)
--test_cintersects = cintersects (Circle (Point 0.0 0.0) 1.0) (Circle (Point 2.0 2.0) 1.0)
--test_vrotate = vrotate (Vector 0.0 1.0) (3.14159/2.0)
--test_circlePolygon = circlePolygon (Circle (Point 0.0 0.0) 1.0)
--test_bboxPolygon = bboxPolygon (Polygon [(Point 0.0 0.0),(Point 1.0 2.0),(Point 2.0 1.0)])

geoutils_tests = TestList [TestLabel "test_padd"  test_padd,
	                   TestLabel "test_vnorm" test_vnorm]