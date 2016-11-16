module OBBTreeTest exposing (testSuite)

import Test exposing (..)
import Expect exposing (..)
import Json.Decode as Decode
import Vector exposing (Vector)
import Quaternion exposing (Quaternion)
import Frame
import Collision.Tree as Tree exposing (Tree(..))
import Collision.Face as Face
import Collision.OBBTree as OBBTree exposing (Body)


testSuite : Test
testSuite =
    describe "Collision detection"
        [ collisionRecursionSuite
        , centeredCollisionSuite
        , offCenterCollisionSuite
        , projectAndSplitSuite
        , jsonSuite
        ]


collisionRecursionSuite : Test
collisionRecursionSuite =
    let
        collide a b =
            OBBTree.collide
                { bounds = a
                , frame = Frame.identity
                }
                { bounds = b
                , frame = Frame.identity
                }

        assertHit a b =
            equal True (collide a b && collide b a)

        assertMiss a b =
            equal False (collide a b || collide b a)

        horzFace ( a, b ) ( c, d ) =
            Leaf
                { p = Vector.vector a b 0
                , q = Vector.vector c d 0
                , r = Vector.vector a d 0
                }

        vertFace ( a, b ) ( c, d ) =
            Leaf
                { p = Vector.vector 0 a b
                , q = Vector.vector 0 c d
                , r = Vector.vector 0 a d
                }

        box ( a, b ) =
            Node
                { a = 5
                , b = 5
                , c = 1
                , frame =
                    { position = Vector.vector a b 0
                    , orientation = Quaternion.identity
                    }
                }
    in
        describe "recursive condition checking"
            [ describe "leaf-leaf collisions"
                [ test "non-colliding leafs do not collide" <|
                    \() ->
                        assertMiss
                            (horzFace ( 1, 1 ) ( 2, 2 ))
                            (vertFace ( -1, 0 ) ( -2, 1 ))
                , test "colliding leafs collide" <|
                    \() ->
                        assertHit
                            (horzFace ( -1, -1 ) ( 1, 1 ))
                            (vertFace ( 1, -1 ) ( -1, 1 ))
                ]
            , describe "leaf-node collisions"
                [ test "miss if leaf collides with just node" <|
                    \() ->
                        assertMiss
                            (horzFace ( 1, 1 ) ( 2, 2 ))
                            (box ( 0, 0 )
                                (vertFace ( 0, 0 ) ( 1, 1 ))
                                (vertFace ( 0, 0 ) ( 1, 1 ))
                            )
                , test "hit if leaf collide with node and left child" <|
                    \() ->
                        assertHit
                            (horzFace ( -1, -1 ) ( 1, 1 ))
                            (box ( 0, 0 )
                                (vertFace ( 1, -1 ) ( -1, 1 ))
                                (vertFace ( -1, -1 ) ( 0, 3 ))
                            )
                , test "hit if leaf collide with node and right child" <|
                    \() ->
                        assertHit
                            (horzFace ( -1, -1 ) ( 1, 1 ))
                            (box ( 0, 0 )
                                (vertFace ( -1, -1 ) ( 0, 3 ))
                                (vertFace ( 1, -1 ) ( -1, 1 ))
                            )
                , test "miss if leaf just collides with children" <|
                    \() ->
                        assertMiss
                            (horzFace ( -1, -1 ) ( 1, 1 ))
                            (box ( -100, 0 )
                                (vertFace ( 1, -1 ) ( -1, 1 ))
                                (vertFace ( 1, -1 ) ( -1, 1 ))
                            )
                ]
            , describe "node-node collisions"
                [ test "false if nodes but no children collide" <|
                    \() ->
                        assertMiss
                            (box ( 1, 1 )
                                (horzFace ( 10, 10 ) ( 11, 11 ))
                                (horzFace ( 12, 12 ) ( 13, 13 ))
                            )
                            (box ( -1, 1 )
                                (vertFace ( -10, 10 ) ( -11, 11 ))
                                (vertFace ( -12, 12 ) ( -13, 13 ))
                            )
                , test "false if children but not nodes collide" <|
                    \() ->
                        assertMiss
                            (box ( -100, 0 )
                                (horzFace ( -1, -1 ) ( 1, 1 ))
                                (vertFace ( 1, -1 ) ( -1, 1 ))
                            )
                            (box ( 100, 0 )
                                (vertFace ( 1, -1 ) ( -1, 1 ))
                                (horzFace ( -1, -1 ) ( 1, 1 ))
                            )
                , test "true if left children collide" <|
                    \() ->
                        assertHit
                            (box ( 1, 1 )
                                (horzFace ( -1, -1 ) ( 1, 1 ))
                                (horzFace ( 12, 12 ) ( 13, 13 ))
                            )
                            (box ( -1, 1 )
                                (vertFace ( 1, -1 ) ( -1, 1 ))
                                (vertFace ( -12, 12 ) ( -13, 13 ))
                            )
                , test "true if left child collides with right child" <|
                    \() ->
                        assertHit
                            (box ( 1, 1 )
                                (horzFace ( -1, -1 ) ( 1, 1 ))
                                (horzFace ( 12, 12 ) ( 13, 13 ))
                            )
                            (box ( -1, 1 )
                                (vertFace ( -12, 12 ) ( -13, 13 ))
                                (vertFace ( 1, -1 ) ( -1, 1 ))
                            )
                , test "true if right children collide" <|
                    \() ->
                        assertHit
                            (box ( 1, 1 )
                                (horzFace ( 12, 12 ) ( 13, 13 ))
                                (horzFace ( -1, -1 ) ( 1, 1 ))
                            )
                            (box ( -1, 1 )
                                (vertFace ( -12, 12 ) ( -13, 13 ))
                                (vertFace ( 1, -1 ) ( -1, 1 ))
                            )
                ]
            ]


{-| collisions where bounding box is centered on the
origin of the body's reference frame
-}
centeredCollisionSuite : Test
centeredCollisionSuite =
    let
        box =
            Node
                { a = 3
                , b = 2
                , c = 1
                , frame = Frame.identity
                }
                (Leaf
                    { p = Vector.vector 3 2 1
                    , q = Vector.vector -3 2 1
                    , r = Vector.vector -3 -2 1
                    }
                )
                (Leaf
                    { p = Vector.vector 3 2 1
                    , q = Vector.vector 3 2 -1
                    , r = Vector.vector -3 2 1
                    }
                )

        assertHit a b =
            equal True
                (OBBTree.collide { a | bounds = box } { b | bounds = box })

        assertMiss a b =
            equal False
                (OBBTree.collide { a | bounds = box } { b | bounds = box })
    in
        describe "Body collisions"
            [ test "bodies that do not collide" <|
                \() ->
                    assertMiss
                        defaultBody
                        (setPosition (Vector.vector 10 0 0) defaultBody)
            , test "aligned bodies that do collide" <|
                \() ->
                    assertHit
                        defaultBody
                        (setPosition (Vector.vector 1 0 0) defaultBody)
            , test "unaligned bodies that do collide" <|
                \() ->
                    assertHit
                        (setOrientation (Quaternion.xRotation (degrees -36.9)) defaultBody)
                        (setOrientation (Quaternion.yRotation (degrees 20.8)) defaultBody)
            , test "unaligned bodies that do not collide" <|
                \() ->
                    assertMiss
                        (defaultBody
                            |> setPosition (Vector.vector 0 0 -2)
                            |> setOrientation (Quaternion.xRotation (degrees -36.8))
                        )
                        (defaultBody
                            |> setPosition (Vector.vector 0 0 2)
                            |> setOrientation (Quaternion.yRotation (degrees 20.7))
                        )
            ]


{-| Bounding box is offset from the body's origin
-}
offCenterCollisionSuite : Test
offCenterCollisionSuite =
    let
        triangle =
            { p = Vector.vector -1 -1 -10
            , q = Vector.vector 1 1 -10
            , r = Vector.vector 0 0 -12
            }

        box =
            { a = 3
            , b = 0.1
            , c = 0.1
            , frame =
                { position = Vector.vector 0 0 -10
                , orientation = Quaternion.yRotation (degrees 45)
                }
            }

        aFrame =
            { position = Vector.vector 0 10 0
            , orientation = Quaternion.xRotation (degrees -90)
            }

        bFrame =
            { position = Vector.vector 10 0 0
            , orientation = Quaternion.yRotation (degrees 90)
            }

        leafTree =
            Leaf triangle

        boxTree =
            Node box (Leaf triangle) (Leaf triangle)
    in
        describe "Off-center body collisions"
            [ test "two faces rotated to the same location" <|
                \() ->
                    equal True
                        (OBBTree.collide
                            { bounds = Leaf triangle, frame = aFrame }
                            { bounds = Leaf triangle, frame = bFrame }
                        )
            , test "two boxes rotated to the same location" <|
                \() ->
                    equal True
                        (OBBTree.collide
                            { bounds = boxTree, frame = aFrame }
                            { bounds = boxTree, frame = bFrame }
                        )
            ]


projectAndSplitSuite : Test
projectAndSplitSuite =
    let
        one =
            Face.face (Vector.vector 1 1 1)
                (Vector.vector 1 1 1)
                (Vector.vector 1 1 1)

        two =
            Face.face (Vector.vector 2 2 2)
                (Vector.vector 2 2 2)
                (Vector.vector 2 2 2)

        three =
            Face.face (Vector.vector 3 3 3)
                (Vector.vector 3 3 3)
                (Vector.vector 3 3 3)

        four =
            Face.face (Vector.vector 4 4 4)
                (Vector.vector 4 4 4)
                (Vector.vector 4 4 4)

        five =
            Face.face (Vector.vector 5 5 5)
                (Vector.vector 5 5 5)
                (Vector.vector 5 5 5)

        facts x face =
            { face = face
            , area = 0
            , center = Vector.vector x 0 0
            }

        projectAndSplit =
            OBBTree.projectAndSplit (Vector.vector 1 0 0)
    in
        describe "Box splitting"
            [ test "split fails if projections are identical" <|
                \() ->
                    equal Nothing
                        (projectAndSplit
                            [ facts 1 one
                            , facts 1 two
                            , facts 1 three
                            , facts 1 four
                            , facts 1 five
                            ]
                        )
            , test "split halfway point if projections are different and list size is even" <|
                \() ->
                    equal (Just ( [ one, two ], [ three, four ] ))
                        (projectAndSplit
                            [ facts 1 one
                            , facts 2 two
                            , facts 3 three
                            , facts 4 four
                            ]
                        )
            , test "split just after halfway point if projections are different and list size is odd" <|
                \() ->
                    equal (Just ( [ one, two, three ], [ four, five ] ))
                        (projectAndSplit
                            [ facts 1 one
                            , facts 2 two
                            , facts 3 three
                            , facts 4 four
                            , facts 5 five
                            ]
                        )
            , test "split by value if 4 items of lower and 1 of higher" <|
                \() ->
                    equal (Just ( [ one, two, three, four ], [ five ] ))
                        (projectAndSplit
                            [ facts 1 one
                            , facts 1 two
                            , facts 1 three
                            , facts 1 four
                            , facts 2 five
                            ]
                        )
            , test "split by value if 3 items of lower and 2 of higher" <|
                \() ->
                    equal (Just ( [ one, two, three ], [ four, five ] ))
                        (projectAndSplit
                            [ facts 1 one
                            , facts 1 two
                            , facts 1 three
                            , facts 2 four
                            , facts 2 five
                            ]
                        )
            , test "split by value if 2 items of lower and 3 of higher" <|
                \() ->
                    equal (Just ( [ one, two ], [ three, four, five ] ))
                        (projectAndSplit
                            [ facts 1 one
                            , facts 1 two
                            , facts 2 three
                            , facts 2 four
                            , facts 2 five
                            ]
                        )
            , test "split by value if 1 item of lower and 4 of higher" <|
                \() ->
                    equal (Just ( [ one ], [ two, three, four, five ] ))
                        (projectAndSplit
                            [ facts 1 one
                            , facts 2 two
                            , facts 2 three
                            , facts 2 four
                            , facts 2 five
                            ]
                        )
            , test "median group goes to first half even if split is very unbalanced" <|
                \() ->
                    equal (Just ( [ one, two, three, four ], [ five ] ))
                        (projectAndSplit
                            [ facts 1 one
                            , facts 1 two
                            , facts 2 three
                            , facts 2 four
                            , facts 3 five
                            ]
                        )
            , test "median group goes to first half even if split is unbalanced" <|
                \() ->
                    equal (Just ( [ one, two, three ], [ four, five ] ))
                        (projectAndSplit
                            [ facts 1 one
                            , facts 2 two
                            , facts 2 three
                            , facts 3 four
                            , facts 3 five
                            ]
                        )
            , test "tolerance in floating point comparisons" <|
                \() ->
                    equal Nothing
                        (projectAndSplit
                            [ facts 0 one
                            , facts 1.0e-10 two
                            , facts -1.0e-10 three
                            ]
                        )
            ]


jsonSuite : Test
jsonSuite =
    let
        assertLosslessJson tree =
            equal
                (Ok tree)
                (Decode.decodeValue OBBTree.decode (OBBTree.encode tree))
    in
        describe "encoding and decoding json"
            [ test "leaf encodes and decodes again without losing data" <|
                \() ->
                    assertLosslessJson
                        (Leaf
                            { p = Vector.vector 1 2 3
                            , q = Vector.vector 4 5 6
                            , r = Vector.vector 7 8 9
                            }
                        )
            , test "node encodes and decodes again without losing data" <|
                \() ->
                    assertLosslessJson
                        (Node
                            { a = 1, b = 2, c = 3, frame = Frame.identity }
                            (Leaf
                                { p = Vector.vector 4 5 6
                                , q = Vector.vector 7 8 9
                                , r = Vector.vector 10 11 12
                                }
                            )
                            (Leaf
                                { p = Vector.vector 13 14 15
                                , q = Vector.vector 16 17 18
                                , r = Vector.vector 19 20 21
                                }
                            )
                        )
            ]


defaultBody : Body {}
defaultBody =
    { frame = Frame.identity
    , bounds = OBBTree.empty
    }


setPosition : Vector -> Body a -> Body a
setPosition p box =
    { box | frame = Frame.setPosition p box.frame }


setOrientation : Quaternion -> Body a -> Body a
setOrientation q box =
    { box | frame = Frame.setOrientation q box.frame }
