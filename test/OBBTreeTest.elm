module OBBTreeTest exposing (testSuite)

import ElmTest exposing (..)
import Vector exposing (Vector)
import Quaternion exposing (Quaternion)
import Frame
import Tree exposing (Tree(..))
import Face as Face
import OBBTree exposing (Body)


testSuite : Test
testSuite =
    suite "Collision detection"
        [ collideSuite
        , projectAndSplitSuite
        ]


collideSuite : Test
collideSuite =
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
            assertEqual True
                (OBBTree.collide { a | bounds = box } { b | bounds = box })

        assertMiss a b =
            assertEqual False
                (OBBTree.collide { a | bounds = box } { b | bounds = box })
    in
        suite "Body collisions"
            [ test "bodies that do not collide" <|
                assertMiss
                    defaultBody
                    (setPosition (Vector.vector 10 0 0) defaultBody)
            , test "aligned bodies that do collide" <|
                assertHit
                    defaultBody
                    (setPosition (Vector.vector 1 0 0) defaultBody)
            , test "unaligned bodies that do collide" <|
                assertHit
                    (setOrientation (Quaternion.rotateX (degrees -36.9)) defaultBody)
                    (setOrientation (Quaternion.rotateY (degrees 20.8)) defaultBody)
            , test "unaligned bodies that do not collide" <|
                assertMiss
                    (defaultBody
                        |> setPosition (Vector.vector 0 0 -2)
                        |> setOrientation (Quaternion.rotateX (degrees -36.8))
                    )
                    (defaultBody
                        |> setPosition (Vector.vector 0 0 2)
                        |> setOrientation (Quaternion.rotateY (degrees 20.7))
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
        suite "Box splitting"
            [ test "split fails if projections are identical" <|
                assertEqual Nothing
                    (projectAndSplit
                        [ facts 1 one
                        , facts 1 two
                        , facts 1 three
                        , facts 1 four
                        , facts 1 five
                        ]
                    )
            , test "split halfway point if projections are different and list size is even" <|
                assertEqual (Just ( [ one, two ], [ three, four ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 2 two
                        , facts 3 three
                        , facts 4 four
                        ]
                    )
            , test "split just after halfway point if projections are different and list size is odd" <|
                assertEqual (Just ( [ one, two, three ], [ four, five ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 2 two
                        , facts 3 three
                        , facts 4 four
                        , facts 5 five
                        ]
                    )
            , test "split by value if 4 items of lower and 1 of higher" <|
                assertEqual (Just ( [ one, two, three, four ], [ five ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 1 two
                        , facts 1 three
                        , facts 1 four
                        , facts 2 five
                        ]
                    )
            , test "split by value if 3 items of lower and 2 of higher" <|
                assertEqual (Just ( [ one, two, three ], [ four, five ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 1 two
                        , facts 1 three
                        , facts 2 four
                        , facts 2 five
                        ]
                    )
            , test "split by value if 2 items of lower and 3 of higher" <|
                assertEqual (Just ( [ one, two ], [ three, four, five ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 1 two
                        , facts 2 three
                        , facts 2 four
                        , facts 2 five
                        ]
                    )
            , test "split by value if 1 item of lower and 4 of higher" <|
                assertEqual (Just ( [ one ], [ two, three, four, five ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 2 two
                        , facts 2 three
                        , facts 2 four
                        , facts 2 five
                        ]
                    )
            , test "median group goes to first half even if split is very unbalanced" <|
                assertEqual (Just ( [ one, two, three, four ], [ five ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 1 two
                        , facts 2 three
                        , facts 2 four
                        , facts 3 five
                        ]
                    )
            , test "median group goes to first half even if split is unbalanced" <|
                assertEqual (Just ( [ one, two, three ], [ four, five ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 2 two
                        , facts 2 three
                        , facts 3 four
                        , facts 3 five
                        ]
                    )
            , test "tolerance in floating point comparisons" <|
                assertEqual Nothing
                    (projectAndSplit
                        [ facts 0 one
                        , facts 1.0e-10 two
                        , facts -1.0e-10 three
                        ]
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
