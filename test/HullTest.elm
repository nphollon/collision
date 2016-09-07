module HullTest exposing (testSuite)

import String
import ElmTest exposing (..)
import Vector exposing (Vector)
import Collision.Face as Face exposing (Face)
import Collision.Hull as Hull


testSuite : Test
testSuite =
    suite "Degenerate hulls"
        [ test "If all points are equal, return empty hull" <|
            assertEqual []
                (Hull.hull
                    [ Vector 1 2 3
                    , Vector 1 2 3
                    , Vector 1 2 3
                    , Vector 1 2 3
                    ]
                )
        , test "If all points are colinear, return the longest possible line segment"
            colinearTest
        , test "If all points are coplanar, return the 2D convex hull"
            coplanarTest
        ]


coplanarTest : Assertion
coplanarTest =
    let
        a =
            Vector -1 -2 -4

        b =
            Vector -2 1 -3

        c =
            Vector 2 3 0

        d =
            Vector 5 0 0
    in
        assertEqual [ Face d b a, Face d c b ]
            (Hull.hull [ a, b, c, d ])


colinearTest : Assertion
colinearTest =
    let
        endA =
            Vector -1 2 7

        midB =
            Vector 0 1 8

        midC =
            Vector 2 -1 10

        endD =
            Vector 3 -2 11

        score pt =
            if Vector.equal endA pt then
                1
            else if Vector.equal endD pt then
                -1
            else
                100

        faceScore face =
            score face.p + score face.q + score face.r

        hull =
            Hull.hull [ midC, endD, endA, midB ]
    in
        case hull of
            face :: [] ->
                if abs (faceScore face) == 1 then
                    pass
                else
                    [ "Expected a face equivalent to segment "
                    , toString ( endA, endD )
                    , ", but actual face was "
                    , toString face
                    ]
                        |> String.concat
                        |> fail

            _ ->
                [ "Expected a hull with only one face, but actual hull was "
                , toString hull
                ]
                    |> String.concat
                    |> fail
