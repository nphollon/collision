module Collision.Tree exposing (Tree(..), leaves, internals, depth, subtreeAt, toTheLeft, toTheRight)

{-| This module defines the tree structure used in the `Bounds` type. You don't need to import this module if you are just doing collision detection. But you may find it useful for debugging.

# Definition

@docs Tree

# Inspecting

@docs depth, leaves, internals, subtreeAt, toTheLeft, toTheRight

-}


{-| A binary tree type. The the internal nodes can store differently-typed data than the leaf nodes.
-}
type Tree a b
    = Leaf b
    | Node a (Tree a b) (Tree a b)


{-| Return the maximum depth of the tree.

    depth (Leaf 1) == 1
    depth (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5)) == 3
-}
depth : Tree a b -> Int
depth tree =
    case tree of
        Leaf _ ->
            1

        Node _ left right ->
            1 + max (depth left) (depth right)


{-| Return a list of leaf values, tagged with tree coordinates.

    leaves (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))
      == [ ((2,0), 3), ((2,1), 4), ((1,1), 5) ]
-}
leaves : Tree a b -> List ( ( Int, Int ), b )
leaves tree =
    leavesRecurse ( 0, 0 ) tree


leavesRecurse : ( Int, Int ) -> Tree a b -> List ( ( Int, Int ), b )
leavesRecurse coords tree =
    case tree of
        Leaf a ->
            [ ( coords, a ) ]

        Node _ left right ->
            leavesRecurse (toTheLeft coords) left
                ++ leavesRecurse (toTheRight coords) right


{-| Return of list of internal node values, tagged with tree coordinates.

    internals (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))
      == [ ((0,0), 1), ((1,0), 2) ]
-}
internals : Tree a b -> List ( ( Int, Int ), a )
internals tree =
    internalsRecurse ( 0, 0 ) tree


internalsRecurse : ( Int, Int ) -> Tree a b -> List ( ( Int, Int ), a )
internalsRecurse coords tree =
    case tree of
        Leaf _ ->
            []

        Node a left right ->
            ( coords, a )
                :: internalsRecurse (toTheLeft coords) left
                ++ internalsRecurse (toTheRight coords) right


{-| Given a pair of tree coordinates, return the coordinates of the left child node.

    toTheLeft (0,0) == (1,0)
    toTheLeft (1,1) == (2,2)
    toTheLeft (4,7) == (5,14)
-}
toTheLeft : ( Int, Int ) -> ( Int, Int )
toTheLeft ( level, offset ) =
    ( level + 1, 2 * offset )


{-| Given a pair of tree coordinates, return the coordinates of the right child node.

    toTheRight (0,0) == (1,1)
    toTheRight (1,1) == (2,3)
    toTheRight (4,7) == (5,15)
-}
toTheRight : ( Int, Int ) -> ( Int, Int )
toTheRight ( level, offset ) =
    ( level + 1, 2 * offset + 1 )


{-| Return the subtree of a given tree, whose root node is at the given coordinates. If the coordinates are out of bounds, return the nearest ancestor.

    subtreeAt (1,0) (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))
      == Node 2 (Leaf 3) (Leaf 4)

    subtreeAt (2,1) (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))
      == Leaf 4

    subtreeAt (2,3) (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))
      == Leaf 5
-}
subtreeAt : ( Int, Int ) -> Tree a b -> Tree a b
subtreeAt ( level, offset ) tree =
    case tree of
        Leaf b ->
            tree

        Node a left right ->
            if level == 0 then
                tree
            else
                let
                    midpoint =
                        2 ^ (level - 1)
                in
                    if offset < midpoint then
                        subtreeAt ( level - 1, offset ) left
                    else
                        subtreeAt ( level - 1, offset - midpoint ) right
