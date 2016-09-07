module CovarianceTest exposing (testSuite)

import ElmTest exposing (..)
import Assertion exposing (..)
import Vector exposing (Vector)
import Quaternion
import Collision.Covariance as Covariance exposing (Basis)


testSuite : Test
testSuite =
    suite "Eigenbasis for a real symmetric matrix"
        [ diagonalSuite
        , nonDiagonalSuite
        , basisToOrientationSuite
        ]


diagonalSuite : Test
diagonalSuite =
    let
        matrix =
            Covariance.init 2 0 0 2 0 2

        eigenbasis =
            Covariance.eigenbasis matrix
    in
        suite "Off-diagonal elements are zero"
            [ test "x component should be non-zero" <|
                assertNotEqual (Vector.vector 0 0 0)
                    eigenbasis.x
            , test "y component should be non-zero" <|
                assertNotEqual (Vector.vector 0 0 0)
                    eigenbasis.y
            ]


nonDiagonalSuite : Test
nonDiagonalSuite =
    let
        matrix =
            Covariance.init -1 1 3 2 0 2

        eigenbasis =
            Covariance.eigenbasis matrix
    in
        suite "Off-diagonal elements are nonzero"
            [ test "one component should be (2, 1, 3) normalized" <|
                assertBasisContains
                    (Vector.scale (1 / sqrt 14) (Vector.vector 2 1 3))
                    eigenbasis
            , test "one component should be (-5, 1, 3) normalized" <|
                assertBasisContains
                    (Vector.scale (1 / sqrt 35) (Vector.vector -5 1 3))
                    eigenbasis
            , test "one component should be (0, -3, 1) normalized" <|
                assertBasisContains
                    (Vector.scale (1 / sqrt 10) (Vector.vector 0 -3 1))
                    eigenbasis
            ]


assertBasisContains : Vector -> Basis -> Assertion
assertBasisContains v basis =
    let
        negV =
            Vector.negate v
    in
        if
            equalVector v basis.x
                || equalVector v basis.y
                || equalVector v basis.z
                || equalVector negV basis.x
                || equalVector negV basis.y
                || equalVector negV basis.z
        then
            pass
        else
            fail ("Expected to find vector " ++ toString v ++ " in basis " ++ toString basis)


basisToOrientationSuite : Test
basisToOrientationSuite =
    suite "converting an orthonormal basis to an axis-angle rotation"
        [ test "Identity transformation" <|
            assertEqualQuaternion
                Quaternion.identity
                (Covariance.basisToQuaternion
                    { x = Vector.vector 1 0 0
                    , y = Vector.vector 0 1 0
                    , z = Vector.vector 0 0 1
                    }
                )
        , test "180 degree rotation" <|
            assertEqualQuaternion
                (Quaternion.fromVector
                    (Vector.vector
                        (turns 0.5 / sqrt 2)
                        (turns 0.5 / sqrt 2)
                        0
                    )
                )
                (Covariance.basisToQuaternion
                    { x = Vector.vector 0 1 0
                    , y = Vector.vector 1 0 0
                    , z = Vector.vector 0 0 -1
                    }
                )
        ]
