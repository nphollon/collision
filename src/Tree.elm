module Tree exposing (Tree(..), satisfies, leaves, encode, decode)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, (:=))


type Tree a b
    = Leaf b
    | Node a (Tree a b) (Tree a b)


satisfies : (a -> a -> Bool) -> (b -> b -> Bool) -> (a -> b -> Bool) -> Tree a b -> Tree a b -> Bool
satisfies checkTwoNodes checkTwoLeafs checkNodeAndLeaf a b =
    case ( a, b ) of
        ( Leaf aVal, Leaf bVal ) ->
            checkTwoLeafs aVal bVal

        ( Leaf aVal, Node bVal bFst bSnd ) ->
            if checkNodeAndLeaf bVal aVal then
                (satisfies checkTwoNodes checkTwoLeafs checkNodeAndLeaf a bFst)
                    || (satisfies checkTwoNodes checkTwoLeafs checkNodeAndLeaf a bSnd)
            else
                False

        ( Node aVal aFst aSnd, Leaf bVal ) ->
            if checkNodeAndLeaf aVal bVal then
                (satisfies checkTwoNodes checkTwoLeafs checkNodeAndLeaf aFst b)
                    || (satisfies checkTwoNodes checkTwoLeafs checkNodeAndLeaf aSnd b)
            else
                False

        ( Node aVal aFst aSnd, Node bVal bFst bSnd ) ->
            if checkTwoNodes aVal bVal then
                (satisfies checkTwoNodes checkTwoLeafs checkNodeAndLeaf aFst bFst)
                    || (satisfies checkTwoNodes checkTwoLeafs checkNodeAndLeaf aFst bSnd)
                    || (satisfies checkTwoNodes checkTwoLeafs checkNodeAndLeaf aSnd bFst)
                    || (satisfies checkTwoNodes checkTwoLeafs checkNodeAndLeaf aSnd bSnd)
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
