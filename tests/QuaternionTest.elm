module QuaternionTest exposing (testSuite)

import Test exposing (..)
import Expect exposing (..)
import Assertion exposing (..)
import Vector as Vec
import Quaternion


testSuite : Test
testSuite =
    describe "Geometric transformations"
        [ rotationForSuite
        , rotateSuite
        ]


rotationForSuite : Test
rotationForSuite =
    describe "Rotating one vector into another"
        [ test "rotation between same vectors is zero" <|
            \() ->
                equal (Quaternion.quaternion 1 0 0 0)
                    (rotationFor (Vec.vector 1 2 3) (Vec.vector 1 2 3))
        , test "rotation between x and y axis is ninety degrees on z axis" <|
            \() ->
                equal (Quaternion.quaternion (cos (degrees 45)) 0 0 (sin (degrees 45)))
                    (rotationFor (Vec.vector 1 0 0) (Vec.vector 0 1 0))
        , test "rotation between vectors ignores their magnitudes" <|
            \() ->
                equal (quaternion (cos (degrees 45)) 0 0 (sin (degrees 45)))
                    (rotationFor (Vec.vector 100 0 0) (Vec.vector 0 100 0))
        ]


rotateSuite : Test
rotateSuite =
    describe "Rotating a vector by a quaternion"
        [ test "rotate x axis ninety degrees on z axis gives y axis" <|
            \() ->
                assertEqualVector (Vec.vector 0 1 0)
                    (rotate (fromVector (Vec.vector 0 0 (turns 0.25)))
                        (Vec.vector 1 0 0)
                    )
        ]
