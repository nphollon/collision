module QuaternionTest exposing (testSuite)

import ElmTest exposing (..)
import Assertion exposing (..)
import Vector as Vec
import Quaternion exposing (..)
import Transform exposing (..)


testSuite : Test
testSuite =
    suite "Geometric transformations"
        [ rotationForSuite
        , basisToOrientationSuite
        , rotateVectorSuite
        ]


rotationForSuite : Test
rotationForSuite =
    suite "Rotating one vector into another"
        [ test "rotation between same vectors is zero"
            <| assertEqual (quaternion 1 0 0 0)
                (rotationFor (Vec.vector 1 2 3) (Vec.vector 1 2 3))
        , test "rotation between x and y axis is ninety degrees on z axis"
            <| assertEqual (quaternion (cos (degrees 45)) 0 0 (sin (degrees 45)))
                (rotationFor (Vec.vector 1 0 0) (Vec.vector 0 1 0))
        , test "rotation between vectors ignores their magnitudes"
            <| assertEqual (quaternion (cos (degrees 45)) 0 0 (sin (degrees 45)))
                (rotationFor (Vec.vector 100 0 0) (Vec.vector 0 100 0))
        ]


basisToOrientationSuite : Test
basisToOrientationSuite =
    suite "converting an orthonormal basis to an axis-angle rotation"
        [ test "Identity transformation"
            <| assertEqualQuaternion (fromVector (Vec.vector 0 0 0))
                (fromBasis
                    { x = Vec.vector 1 0 0
                    , y = Vec.vector 0 1 0
                    , z = Vec.vector 0 0 1
                    }
                )
        , test "180 degree rotation"
            <| assertEqualQuaternion (fromVector (Vec.vector (turns 0.5 / sqrt 2) (turns 0.5 / sqrt 2) 0))
                (fromBasis
                    { x = Vec.vector 0 1 0
                    , y = Vec.vector 1 0 0
                    , z = Vec.vector 0 0 -1
                    }
                )
        ]


rotateVectorSuite : Test
rotateVectorSuite =
    suite "Rotating a vector by a quaternion"
        [ test "rotate x axis ninety degrees on z axis gives y axis"
            <| assertEqualVector (Vec.vector 0 1 0)
                (rotateVector (fromVector (Vec.vector 0 0 (turns 0.25)))
                    (Vec.vector 1 0 0)
                )
        ]
