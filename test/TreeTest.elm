module TreeTest exposing (testSuite)

import ElmTest exposing (..)
import Collision.Tree as Tree exposing (Tree(..))


testSuite : Test
testSuite =
    suite "Tree structure"
        [ leavesSuite
        , internalSuite
        , depthSuite
        ]


leavesSuite : Test
leavesSuite =
    suite "getting the leaves of a tree"
        [ test "Return leaf value as singleton list" <|
            assertEqual [ ( ( 0, 0 ), 5 ) ]
                (Tree.leaves (Leaf 5))
        , test "Return both leaves of a three-node tree" <|
            assertEqual [ ( ( 1, 0 ), 7 ), ( ( 1, 1 ), 8 ) ]
                (Tree.leaves (Node 9 (Leaf 7) (Leaf 8)))
        , test "Concatenate all leaves of a multi-level tree" <|
            assertEqual [ ( ( 1, 0 ), 1 ), ( ( 2, 2 ), 3 ), ( ( 2, 3 ), 2 ) ]
                (Tree.leaves (Node 0 (Leaf 1) (Node 4 (Leaf 3) (Leaf 2))))
        ]


internalSuite : Test
internalSuite =
    suite "getting the internal nodes of a tree"
        [ test "Return leaf value as empty list" <|
            assertEqual []
                (Tree.internals (Leaf 5))
        , test "Return the parent node of a three-node tree" <|
            assertEqual [ ( ( 0, 0 ), 9 ) ]
                (Tree.internals (Node 9 (Leaf 7) (Leaf 8)))
        , test "Concatenate all internal nodes of a multi-level tree" <|
            assertEqual [ ( ( 0, 0 ), 0 ), ( ( 1, 1 ), 4 ) ]
                (Tree.internals (Node 0 (Leaf 1) (Node 4 (Leaf 3) (Leaf 2))))
        ]


depthSuite : Test
depthSuite =
    suite "measuring tree depth"
        [ test "depth of leaf is 1" <|
            assertEqual 1 (Tree.depth (Leaf 1))
        , test "depth of 3-node tree is 2" <|
            assertEqual 2
                (Tree.depth (Node 1 (Leaf 2) (Leaf 3)))
        , test "depth of 5-node tree is 3" <|
            assertEqual 3
                (Tree.depth (Node 1 (Leaf 2) (Node 3 (Leaf 4) (Leaf 5))))
        ]
