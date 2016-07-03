module Tree exposing (Tree(..), satisfies, leaves, encode, decode)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, (:=))


type Tree a b
    = Leaf b
    | Node a (Tree a b) (Tree a b)


satisfies : (a -> c -> Bool) -> (b -> d -> Bool) -> (a -> d -> Bool) -> (b -> c -> Bool) -> Tree a b -> Tree c d -> Bool
satisfies nodeNode leafLeaf nodeLeaf leafNode a b =
    case ( a, b ) of
        ( Leaf aVal, Leaf bVal ) ->
            leafLeaf aVal bVal

        ( Leaf aVal, Node bVal bFst bSnd ) ->
            if leafNode aVal bVal then
                (satisfies nodeNode leafLeaf nodeLeaf leafNode a bFst)
                    || (satisfies nodeNode leafLeaf nodeLeaf leafNode a bSnd)
            else
                False

        ( Node aVal aFst aSnd, Leaf bVal ) ->
            if nodeLeaf aVal bVal then
                (satisfies nodeNode leafLeaf nodeLeaf leafNode aFst b)
                    || (satisfies nodeNode leafLeaf nodeLeaf leafNode aSnd b)
            else
                False

        ( Node aVal aFst aSnd, Node bVal bFst bSnd ) ->
            if nodeNode aVal bVal then
                (satisfies nodeNode leafLeaf nodeLeaf leafNode aFst bFst)
                    || (satisfies nodeNode leafLeaf nodeLeaf leafNode aFst bSnd)
                    || (satisfies nodeNode leafLeaf nodeLeaf leafNode aSnd bFst)
                    || (satisfies nodeNode leafLeaf nodeLeaf leafNode aSnd bSnd)
            else
                False


leaves : Tree a b -> List b
leaves tree =
    case tree of
        Leaf a ->
            [ a ]

        Node _ left right ->
            leaves left ++ leaves right


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
