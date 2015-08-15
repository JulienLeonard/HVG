module Test_Geoutils where

import Mathutils
import Geoutils
import Test.HUnit

test_padd          = TestCase (assertEqual "test padd"          (Point 4.0 6.0) (padd (Point 1.0 2.0) (Vector 3.0 4.0)))
test_vnorm         = TestCase (assertEqual "test vnorm"         (Vector 0.0 1.0)  (vnorm (Vector 0.0 2.0)))
test_cradius       = TestCase (assertEqual "test cradius"       1.5 (cradius (Circle (Point 0.0 0.0) 1.5)))
test_ccenter       = TestCase (assertEqual "test ccenter"       (Point 0.0 0.1) (ccenter (Circle (Point 0.0 0.1) 1.5)))
test_vector        = TestCase (assertEqual "test vector"        (Vector 2.0 3.0) (vector (Point 1.0 2.0) (Point 3.0 5.0)))
test_cintersects   = TestCase (assertEqual "test cintersects"   False (cintersects (Circle (Point 0.0 0.0) 1.0) (Circle (Point 2.0 2.0) 1.0)))
test_vrotate       = TestCase (assertEqual "test vrotate"       True (0.0001 > (vdist (vsub (Vector (0.0) (-1.0)) (vrotate (Vector 0.0 1.0) (sample rangeangle 0.5))))))
test_circlePolygon = TestCase (assertEqual "test circlePolygon" 100 (length (ppoints (circlePolygon (Circle (Point 0.0 0.0) 1.0)))))
test_bboxPolygon   = TestCase (assertEqual "test bboxPolygon"   (BBox (Point 0.0 0.0) (Point 2.0 2.0)) (bboxPolygon (Polygon [(Point 0.0 0.0),(Point 1.0 2.0),(Point 2.0 1.0)])))

geoutils_tests = TestList [TestLabel "test_padd"          test_padd,
	                   TestLabel "test_vnorm"         test_vnorm,
			   TestLabel "test_cradius"       test_cradius,
			   TestLabel "test_vector"        test_vector,
			   TestLabel "test_cintersects"   test_cintersects,
			   TestLabel "test_vrotate"       test_vrotate,
			   TestLabel "test_circlePolygon" test_circlePolygon,
			   TestLabel "test_bboxPolygon"   test_bboxPolygon]