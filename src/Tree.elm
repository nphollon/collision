module Tree exposing (Tree(..), CrossFunctions, satisfies, leaves, internals, depth, encode, decode, subtreeAt, toTheLeft, toTheRight, collisionMap)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, (:=))
import Set exposing (Set)


type Tree a b
    = Leaf b
    | Node a (Tree a b) (Tree a b)


depth : Tree a b -> Int
depth tree =
    case tree of
        Leaf _ ->
            1

        Node _ left right ->
            1 + max (depth left) (depth right)


type alias CrossFunctions a b c d =
    { nodeNode : a -> c -> Bool
    , leafLeaf : b -> d -> Bool
    , nodeLeaf : a -> d -> Bool
    , leafNode : b -> c -> Bool
    }


satisfies : CrossFunctions a b c d -> Tree a b -> Tree c d -> Bool
satisfies xf a b =
    case ( a, b ) of
        ( Leaf aVal, Leaf bVal ) ->
            xf.leafLeaf aVal bVal

        ( Leaf aVal, Node bVal bFst bSnd ) ->
            xf.leafNode aVal bVal
                && (satisfies xf a bFst || satisfies xf a bSnd)

        ( Node aVal aFst aSnd, Leaf bVal ) ->
            xf.nodeLeaf aVal bVal
                && (satisfies xf aFst b || satisfies xf aSnd b)

        ( Node aVal aFst aSnd, Node bVal bFst bSnd ) ->
            xf.nodeNode aVal bVal
                && (satisfies xf aFst bFst
                        || satisfies xf aFst bSnd
                        || satisfies xf aSnd bFst
                        || satisfies xf aSnd bSnd
                   )


collisionMap : CrossFunctions a b c d -> Tree a b -> Tree c d -> Set ( Int, Int )
collisionMap xf a b =
    fold xf a b ( 0, 0 ) Set.empty


fold : CrossFunctions a b c d -> Tree a b -> Tree c d -> ( Int, Int ) -> Set ( Int, Int ) -> Set ( Int, Int )
fold xf a b coords hits =
    case ( a, b ) of
        ( Leaf aVal, Leaf bVal ) ->
            if xf.leafLeaf aVal bVal then
                Set.insert coords hits
            else
                hits

        ( Leaf aVal, Node bVal bLeft bRight ) ->
            if xf.leafNode aVal bVal then
                hits
                    |> fold xf a bLeft coords
                    |> fold xf a bRight coords
            else
                hits

        ( Node aVal aLeft aRight, Leaf bVal ) ->
            if xf.nodeLeaf aVal bVal then
                Set.insert coords hits
                    |> fold xf aLeft b (toTheLeft coords)
                    |> fold xf aRight b (toTheRight coords)
            else
                hits

        ( Node aVal aLeft aRight, Node bVal bLeft bRight ) ->
            if xf.nodeNode aVal bVal then
                Set.insert coords hits
                    |> fold xf aLeft bLeft (toTheLeft coords)
                    |> fold xf aLeft bRight (toTheLeft coords)
                    |> fold xf aRight bLeft (toTheRight coords)
                    |> fold xf aRight bRight (toTheRight coords)
            else
                hits


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


toTheLeft : ( Int, Int ) -> ( Int, Int )
toTheLeft ( level, offset ) =
    ( level + 1, 2 * offset )


toTheRight : ( Int, Int ) -> ( Int, Int )
toTheRight ( level, offset ) =
    ( level + 1, 2 * offset + 1 )


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


encode : (a -> Value) -> (b -> Value) -> Tree a b -> Value
encode nodeEncoder leafEncoder tree =
    case tree of
        Leaf a ->
            Encode.object
                [ ( "nodeType", Encode.string "leaf" )
                , ( "value", leafEncoder a )
                ]

        Node a left right ->
            Encode.object
                [ ( "nodeType", Encode.string "internal" )
                , ( "value", nodeEncoder a )
                , ( "left", encode nodeEncoder leafEncoder left )
                , ( "right", encode nodeEncoder leafEncoder right )
                ]


decode : Decoder a -> Decoder b -> Decoder (Tree a b)
decode nodeDecoder leafDecoder =
    let
        treeData nodeType =
            case nodeType of
                "leaf" ->
                    Decode.object1 Leaf ("value" := leafDecoder)

                "internal" ->
                    Decode.object3 Node
                        ("value" := nodeDecoder)
                        ("left" := decode nodeDecoder leafDecoder)
                        ("right" := decode nodeDecoder leafDecoder)

                _ ->
                    Decode.fail ("unrecognized node type: " ++ nodeType)
    in
        Decode.andThen ("nodeType" := Decode.string) treeData
