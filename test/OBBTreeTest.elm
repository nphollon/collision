module OBBTreeTest exposing (testSuite)

import ElmTest exposing (..)
import Vector exposing (Vector)
import Quaternion exposing (Quaternion)
import Tree exposing (Tree(..))
import Face as Face
import OBBTree exposing (Body)
import BoundingBox as BoundingBox exposing (BoundingBox)


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
            Leaf boxA

        assertHit a b =
            assertEqual True
                (OBBTree.collide { a | bounds = Just box } { b | bounds = Just box })

        assertMiss a b =
            assertEqual False
                (OBBTree.collide { a | bounds = Just box } { b | bounds = Just box })
    in
        suite "Body collisions"
            [ test "bodies that do not collide"
                <| assertMiss
                    { defaultBody
                        | position = Vector.vector 0 0 0
                    }
                    { defaultBody
                        | position = Vector.vector 10 0 0
                    }
            , test "aligned bodies that do collide"
                <| assertHit
                    { defaultBody
                        | position = Vector.vector 0 0 0
                    }
                    { defaultBody
                        | position = Vector.vector 1 0 0
                    }
            , test "unaligned bodies that do collide"
                <| assertHit
                    { defaultBody
                        | position = Vector.vector 0 0 -2
                        , orientation = Quaternion.fromVector (Vector.vector (degrees -36.9) 0 0)
                    }
                    { defaultBody
                        | position = Vector.vector 0 0 2
                        , orientation = Quaternion.fromVector (Vector.vector 0 (degrees 20.8) 0)
                    }
            , test "unaligned bodies that do not collide"
                <| assertMiss
                    { defaultBody
                        | position = Vector.vector 0 0 -2
                        , orientation = Quaternion.fromVector (Vector.vector (degrees -36.8) 0 0)
                    }
                    { defaultBody
                        | position = Vector.vector 0 0 2
                        , orientation = Quaternion.fromVector (Vector.vector 0 (degrees 20.7) 0)
                    }
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
            [ test "split fails if projections are identical"
                <| assertEqual Nothing
                    (projectAndSplit
                        [ facts 1 one
                        , facts 1 two
                        , facts 1 three
                        , facts 1 four
                        , facts 1 five
                        ]
                    )
            , test "split halfway point if projections are different and list size is even"
                <| assertEqual (Just ( [ one, two ], [ three, four ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 2 two
                        , facts 3 three
                        , facts 4 four
                        ]
                    )
            , test "split just after halfway point if projections are different and list size is odd"
                <| assertEqual (Just ( [ one, two, three ], [ four, five ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 2 two
                        , facts 3 three
                        , facts 4 four
                        , facts 5 five
                        ]
                    )
            , test "split by value if 4 items of lower and 1 of higher"
                <| assertEqual (Just ( [ one, two, three, four ], [ five ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 1 two
                        , facts 1 three
                        , facts 1 four
                        , facts 2 five
                        ]
                    )
            , test "split by value if 3 items of lower and 2 of higher"
                <| assertEqual (Just ( [ one, two, three ], [ four, five ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 1 two
                        , facts 1 three
                        , facts 2 four
                        , facts 2 five
                        ]
                    )
            , test "split by value if 2 items of lower and 3 of higher"
                <| assertEqual (Just ( [ one, two ], [ three, four, five ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 1 two
                        , facts 2 three
                        , facts 2 four
                        , facts 2 five
                        ]
                    )
            , test "split by value if 1 item of lower and 4 of higher"
                <| assertEqual (Just ( [ one ], [ two, three, four, five ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 2 two
                        , facts 2 three
                        , facts 2 four
                        , facts 2 five
                        ]
                    )
            , test "median group goes to first half even if split is very unbalanced"
                <| assertEqual (Just ( [ one, two, three, four ], [ five ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 1 two
                        , facts 2 three
                        , facts 2 four
                        , facts 3 five
                        ]
                    )
            , test "median group goes to first half even if split is unbalanced"
                <| assertEqual (Just ( [ one, two, three ], [ four, five ] ))
                    (projectAndSplit
                        [ facts 1 one
                        , facts 2 two
                        , facts 2 three
                        , facts 3 four
                        , facts 3 five
                        ]
                    )
            , test "tolerance in floating point comparisons"
                <| assertEqual Nothing
                    (projectAndSplit
                        [ facts 0 one
                        , facts 1.0e-10 two
                        , facts -1.0e-10 three
                        ]
                    )
            ]


defaultBody : Body {}
defaultBody =
    { position = Vector.vector 0 0 0
    , orientation = Quaternion.fromVector (Vector.vector 0 0 0)
    , bounds = Nothing
    }


boxA : BoundingBox
boxA =
    { a = 3
    , b = 2
    , c = 1
    , position = Vector.vector 0 0 0
    , orientation = Quaternion.fromVector (Vector.vector 0 0 0)
    }


boxB : BoundingBox
boxB =
    let
        comp =
            pi / 6 / sqrt 3
    in
        { a = 3
        , b = 2
        , c = 1
        , position = Vector.vector 0 0 0
        , orientation = Quaternion.fromVector (Vector.vector comp comp comp)
        }
