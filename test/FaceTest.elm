module FaceTest exposing (..)

import ElmTest exposing (..)
import Json.Decode as Decode
import Face exposing (Face)
import Vector


testSuite : Test
testSuite =
    suite "Triangular faces"
        [ intersectionSuite
        , jsonSuite
        ]


intersectionSuite : Test
intersectionSuite =
    let
        stationaryTriangle =
            { p = Vector.vector -1 0 0
            , q = Vector.vector 0 1 0
            , r = Vector.vector 0 -1 0
            }
    in
        suite "Face intersections"
            [ testIntersection "Triangles intersect if their edges pierce each other"
                True
                stationaryTriangle
                { p = Vector.vector -0.01 0 -1
                , q = Vector.vector -0.01 0 1
                , r = Vector.vector 0.5 0 0
                }
            , testIntersection "Triangles intersect if one vertex pierces a face"
                True
                stationaryTriangle
                { p = Vector.vector -1 0 1
                , q = Vector.vector 0 0 1
                , r = Vector.vector 0 0 -0.01
                }
            , testIntersection "Triangles don't intersect if edges don't touch"
                False
                stationaryTriangle
                { p = Vector.vector 0.01 0 -1
                , q = Vector.vector 0.01 0 1
                , r = Vector.vector 0.5 0 0
                }
            , testIntersection "Triangles don't intersect if vertex doesn't pierce face"
                False
                stationaryTriangle
                { p = Vector.vector -1 0 1
                , q = Vector.vector 0 0 1
                , r = Vector.vector 0 0 0.01
                }
            ]


testIntersection : String -> Bool -> Face -> Face -> Test
testIntersection name shouldIntersect faceA faceB =
    suite name
        [ testAllPermutations "triangles in given order" shouldIntersect faceA faceB
        , testAllPermutations "triangles in reverse order" shouldIntersect faceB faceA
        ]


testAllPermutations : String -> Bool -> Face -> Face -> Test
testAllPermutations name shouldIntersect a b =
    let
        subTest subName aPermuted =
            testBPermutations subName shouldIntersect aPermuted b
    in
        suite name
            [ subTest "A: p q r" { p = a.p, q = a.q, r = a.r }
            , subTest "A: q r p" { p = a.q, q = a.r, r = a.p }
            , subTest "A: r p q" { p = a.r, q = a.p, r = a.q }
            , subTest "A: r q p" { p = a.r, q = a.q, r = a.p }
            , subTest "A: q p r" { p = a.q, q = a.p, r = a.r }
            , subTest "A: p r q" { p = a.p, q = a.r, r = a.q }
            ]


testBPermutations : String -> Bool -> Face -> Face -> Test
testBPermutations name shouldIntersect a b =
    let
        subTest subName bPermuted =
            test subName
                <| assertEqual shouldIntersect
                    (Face.collide a bPermuted)
    in
        suite name
            [ subTest "B: p q r" { p = b.p, q = b.q, r = b.r }
            , subTest "B: q r p" { p = b.q, q = b.r, r = b.p }
            , subTest "B: r p q" { p = b.r, q = b.p, r = b.q }
            , subTest "B: r q p" { p = b.r, q = b.q, r = b.p }
            , subTest "B: q p r" { p = b.q, q = b.p, r = b.r }
            , subTest "B: p r q" { p = b.p, q = b.r, r = b.q }
            ]


jsonSuite : Test
jsonSuite =
    let
        face =
            { p = Vector.vector 10 11 12
            , q = Vector.vector 9 8 7
            , r = Vector.vector -1 0 1
            }
    in
        test "Json encoding & decoding"
            <| assertEqual (Ok face)
                (Decode.decodeValue Face.decode (Face.encode face))
