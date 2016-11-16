module BoundingBoxTest exposing (testSuite)

import Test exposing (..)
import Expect exposing (..)
import Assertion exposing (..)
import Json.Decode as Decode
import Vector exposing (Vector)
import Quaternion exposing (Quaternion)
import Frame exposing (Frame)
import Collision.Face as Face exposing (Face)
import Collision.BoundingBox as BoundingBox exposing (BoundingBox)


testSuite : Test
testSuite =
    describe "Bounding box primitive"
        [ creationSuite
        , collideWithBoxSuite
        , collideWithFaceSuite
        , jsonSuite
        ]


creationSuite : Test
creationSuite =
    let
        bounds =
            BoundingBox.create cube
    in
        describe "Creating a bounding box for a cube"
            [ test "Centered at origin" <|
                \() ->
                    assertEqualVector
                        (Vector.vector 0 0 0)
                        bounds.frame.position
            , test "Major radius of 1" <|
                \() ->
                    assertEqualFloat
                        3.6
                        bounds.a
            , test "Middle radius of 1" <|
                \() ->
                    assertEqualFloat
                        3.6
                        bounds.b
            , test "Minor radius of 1" <|
                \() ->
                    assertEqualFloat
                        3.6
                        bounds.c
            ]


cube : List Face
cube =
    let
        one =
            Vector.vector 3.6 -3.6 -3.6

        two =
            Vector.vector 3.6 -3.6 3.6

        three =
            Vector.vector -3.6 -3.6 3.599999

        four =
            Vector.vector -3.599998 -3.6 -3.600001

        five =
            Vector.vector 3.600002 3.6 -3.599998

        six =
            Vector.vector 3.599998 3.6 3.600002

        seven =
            Vector.vector -3.600001 3.6 3.599998

        eight =
            Vector.vector -3.5999999 3.6 -3.6
    in
        [ Face.face one two three
        , Face.face one three four
        , Face.face five eight seven
        , Face.face five seven six
        , Face.face one five six
        , Face.face one six two
        , Face.face two six seven
        , Face.face two seven three
        , Face.face three seven eight
        , Face.face three eight four
        , Face.face five one four
        , Face.face five four eight
        ]


collideWithBoxSuite : Test
collideWithBoxSuite =
    let
        assertMiss v =
            \() ->
                equal False
                    (BoundingBox.collide boxA (setPosition v boxB))
    in
        describe "Collsion between oriented bounding boxes"
            [ test "concentric boxes collide" <|
                \() ->
                    equal True
                        (BoundingBox.collide boxA boxB)
            , test "nested boxes collide" <|
                \() ->
                    equal True
                        (BoundingBox.collide boxA littleBox)
            , describe "no collision on face axis projections"
                [ test "A major axis" <|
                    assertMiss (Vector.vector 6.7 0 0)
                , test "A secondary axis" <|
                    assertMiss (Vector.vector 0 5.07 0)
                , test "A minor axis" <|
                    assertMiss (Vector.vector 0 0 3.33)
                , test "B major axis" <|
                    assertMiss (Vector.vector 6.1 2.2 -1.6)
                , test "B secondary axis" <|
                    assertMiss (Vector.vector -1.2 4.6 1.7)
                , test "B minor axis" <|
                    assertMiss (Vector.vector 1.2 -1.2 3)
                ]
            , describe "no collision on edge-pair axis projections"
                [ test "A major x B major" <|
                    assertMiss (Vector.vector 0 4 3)
                , test "A major x B middle" <|
                    assertMiss (Vector.vector -1 -3 3)
                , test "A major x B minor" <|
                    assertMiss (Vector.vector 0 5 1)
                , test "A middle x B major" <|
                    assertMiss (Vector.vector 5 4 2.2)
                , test "A middle x B middle" <|
                    assertMiss (Vector.vector 6.5 0 1)
                , test "A middle x B minor" <|
                    assertMiss (Vector.vector 6.5 0 -2)
                , test "A minor x B major" <|
                    assertMiss (Vector.vector 6 -3.5 0)
                , test "A minor x B middle" <|
                    assertMiss (Vector.vector 6 3.5 0)
                , test "A minor x B minor" <|
                    assertMiss (Vector.vector 5.1 5 -1)
                ]
            , describe "Moving box A"
                [ test "collision with box A translated" <|
                    \() ->
                        equal True
                            (BoundingBox.collide
                                (setPosition (Vector.vector 0.2 0.2 0.2) boxA)
                                (setPosition (Vector.vector 5 4 2.2) boxB)
                            )
                , test "collision with box A rotated" <|
                    \() ->
                        equal True
                            (BoundingBox.collide
                                boxB
                                (setPosition (Vector.vector 0 2 1.2) boxA)
                            )
                ]
            , describe "degenerate cases"
                [ test "collision when boxes are aligned" <|
                    \() ->
                        equal True
                            (BoundingBox.collide
                                (setPosition (Vector.vector 0 1 0) boxA)
                                (setPosition (Vector.vector 0 -1 0) boxA)
                            )
                ]
            ]


collideWithFaceSuite : Test
collideWithFaceSuite =
    let
        collideWithFace =
            BoundingBox.collideWithFace
                { p = Vector.vector -2.9 -1.9 -0.9
                , q = Vector.vector 2.9 1.9 0.9
                , r = Vector.vector 2.9 1.9 -0.9
                }
    in
        describe "Collision between oriented bounding box and triangular face"
            [ test "collision when box encloses triangle" <|
                \() ->
                    equal True
                        (collideWithFace boxA)
            , test "collision with +A box face" <|
                \() ->
                    equal True
                        (collideWithFace (setPosition (Vector.vector -0.2 0 0) boxA))
            , test "collision with -A box face" <|
                \() ->
                    equal True
                        (collideWithFace (setPosition (Vector.vector 0.2 0 0) boxA))
            , test "collision with +B box face" <|
                \() ->
                    equal True
                        (collideWithFace (setPosition (Vector.vector 0 -0.2 0) boxA))
            , test "collision with -B box face" <|
                \() ->
                    equal True
                        (collideWithFace (setPosition (Vector.vector 0 0.2 0) boxA))
            , test "collision with +C box face" <|
                \() ->
                    equal True
                        (collideWithFace (setPosition (Vector.vector 0 0 -0.2) boxA))
            , test "collision with -C box face" <|
                \() ->
                    equal True
                        (collideWithFace (setPosition (Vector.vector 0 0 0.2) boxA))
            , test "collision with rotated box" <|
                \() ->
                    equal True
                        (collideWithFace
                            (setOrientation (Quaternion.xRotation (turns 0.25)) boxA)
                        )
            , test "no collision when box away from triangle" <|
                \() ->
                    equal False
                        (collideWithFace (setPosition (Vector.vector 3.1 -2 0) boxA))
            ]


boxA : BoundingBox
boxA =
    { a = 3
    , b = 2
    , c = 1
    , frame = Frame.identity
    }


littleBox : BoundingBox
littleBox =
    { a = 0.5
    , b = 0.5
    , c = 0.5
    , frame = Frame.identity
    }


boxB : BoundingBox
boxB =
    let
        h =
            2 - sqrt 3
    in
        { a = 3
        , b = 2
        , c = 1
        , frame =
            { position = Vector.identity
            , orientation =
                Quaternion.quaternion (sqrt 3) h h h
            }
        }


setPosition : Vector -> BoundingBox -> BoundingBox
setPosition p box =
    { box | frame = Frame.setPosition p box.frame }


setOrientation : Quaternion -> BoundingBox -> BoundingBox
setOrientation q box =
    { box | frame = Frame.setOrientation q box.frame }


jsonSuite : Test
jsonSuite =
    let
        box =
            { a = 1.5
            , b = 2.5
            , c = 3.5
            , frame =
                { position = Vector.vector -1 -2 -3
                , orientation = Quaternion.quaternion 0.4 0.6 0 0
                }
            }
    in
        test "Json encoding & decoding" <|
            \() ->
                equal (Ok box)
                    (Decode.decodeValue BoundingBox.decode (BoundingBox.encode box))
