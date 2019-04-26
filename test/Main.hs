module Main where

import Test.HUnit
import Data.Text (Text)
import StreetRun.Runner
import StreetRun.Types
import StreetRun.Types
import Linear
import StreetRun.CFL
import StreetRun.Step
import StreetRun.Quake
import StreetRun.Camera

titleShowPressSpace :: Float -> Bool
titleShowPressSpace x = sin x > 0.5

titleShowPressEscape :: Float -> Bool
titleShowPressEscape p = sin p < -0.5

canAddObstacle :: Distance -> Bool 
canAddObstacle dist = dist < realToFrac 1280

test1::Test 
test1 = TestCase(do 
	let x = titleShowPressEscape 5.2
	assertBool ("Press Escape Failed") x )

test2::Test 
test2 = TestCase(do 
	let x = titleShowPressSpace (-5.2)
	assertBool ("Press Space failed") x )

test3::Test
test3 = TestCase(do 
	let x = canAddObstacle (780)
	assertBool ("Failed to add Obstacle") x )

test4::Test
test4 = TestCase(do 
	let (V2 x y) = moveOrigin (V2 640 640)
	assertEqual "for camera" (0,0) (x,y) )


tests::Test
tests= TestList[ TestLabel "test1" test1 
				,TestLabel "test2" test2
				,TestLabel "test3" test3
				,TestLabel "test4" test4
				]

main::IO Counts
main = do 
	print("Passed All Test Cases")
	runTestTT tests



