module DataView exposing (draw)

import Set exposing (Set)
import String
import Html exposing (Html)
import Html.Attributes as Attr
import Html.App as App
import Html.Events as Evt
import InlineHover as Hov


-- Collision Library

import Collision exposing (Body, Bounds, Vector, Quaternion)
import OBBTree
import Tree exposing (Tree(..))


-- Project Local

import Types exposing (..)
import Elements


draw : Model -> Html Action
draw model =
    let
        titleStyle =
            Attr.style
                [ ( "font-size", "1.2rem" )
                , ( "font-weight", "normal" )
                , ( "text-align", "center" )
                ]

        redHits =
            OBBTree.collisionMap model.red model.blue

        blueHits =
            OBBTree.collisionMap model.blue model.red
    in
        Html.div
            [ Attr.style [ ( "width", "750px" ) ] ]
            [ Html.h2 [ titleStyle ] [ Html.text "Red" ]
            , Elements.divider
            , App.map (SelectNode Red) (displayBody model.red redHits)
            , Html.h2 [ titleStyle ] [ Html.text "Blue" ]
            , Elements.divider
            , App.map (SelectNode Blue) (displayBody model.blue blueHits)
            ]


displayBody : Entity -> Set ( Int, Int ) -> Html ( Int, Int )
displayBody entity hits =
    let
        position =
            entity.frame.position

        orientation =
            entity.frame.orientation

        selectedSubtree =
            Tree.subtreeAt entity.selectedNode entity.bounds
    in
        Html.div
            [ Attr.style
                [ ( "display", "flex" )
                , ( "flex-wrap", "wrap" )
                , ( "justify-content", "center" )
                ]
            ]
            [ Html.div
                [ Attr.style
                    [ ( "line-height", "2rem" )
                    , ( "width", "500px" )
                    ]
                ]
                [ vectorDetails "Position" entity.frame.position
                , quaternionDetails "Orientation" entity.frame.orientation
                , subtreeDetails selectedSubtree
                ]
            , Html.div
                [ Attr.style
                    [ ( "margin-top", "1rem" )
                    , ( "margin-bottom", "2rem" )
                    ]
                ]
                [ drawTree
                    hits
                    entity.selectedNode
                    ( 0, 0 )
                    entity.bounds
                ]
            ]


subtreeDetails : Bounds -> Html a
subtreeDetails tree =
    case tree of
        Leaf face ->
            Html.div []
                [ vectorDetails "Vertex 1" face.p
                , vectorDetails "Vertex 2" face.q
                , vectorDetails "Vertex 3" face.r
                ]

        Node box _ _ ->
            Html.div []
                [ line "Radius"
                    [ ( "A", box.a ), ( "B", box.b ), ( "C", box.c ) ]
                , vectorDetails "Offset" box.frame.position
                , quaternionDetails "Rotation" box.frame.orientation
                ]


vectorDetails : String -> Vector -> Html a
vectorDetails label position =
    line label
        [ ( "X", position.x )
        , ( "Y", position.y )
        , ( "Z", position.z )
        ]


quaternionDetails : String -> Quaternion -> Html a
quaternionDetails label orientation =
    line label
        [ ( "W", orientation.scalar )
        , ( "X", orientation.vector.x )
        , ( "Y", orientation.vector.y )
        , ( "Z", orientation.vector.z )
        ]


line : String -> List ( String, Float ) -> Html a
line label lineItems =
    let
        fields =
            List.map (uncurry field) lineItems
    in
        Html.div
            [ Attr.style
                [ ( "display", "flex" )
                , ( "justify-content", "space-around" )
                ]
            ]
            (Html.em [] [ Html.text label ] :: fields)


field : String -> Float -> Html a
field label value =
    Html.div []
        [ Html.text label
        , Html.text " = "
        , Html.text (float value)
        ]


drawTree : Set ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> Bounds -> Html ( Int, Int )
drawTree hits selected coords tree =
    let
        nodeType =
            if coords == selected then
                Selected
            else if Set.member coords hits then
                Hit
            else
                NoHit
    in
        case tree of
            Leaf b ->
                Html.div
                    [ Attr.style
                        [ ( "display", "flex" )
                        , ( "justify-content", "center" )
                        ]
                    ]
                    [ nodeBox nodeType coords ]

            Node a left right ->
                Html.div
                    [ Attr.style
                        [ ( "display", "flex" )
                        , ( "flex-wrap", "wrap" )
                        , ( "justify-content", "center" )
                        ]
                    ]
                    [ nodeBox nodeType coords
                    , branches
                    , drawChild hits selected (Tree.toTheLeft coords) left
                    , drawChild hits selected (Tree.toTheRight coords) right
                    ]


type NodeType
    = Selected
    | Hit
    | NoHit


nodeBox : NodeType -> ( Int, Int ) -> Html ( Int, Int )
nodeBox nodeType coords =
    let
        color =
            case nodeType of
                Selected ->
                    "blue"

                Hit ->
                    "#ffaa00"

                NoHit ->
                    "#999999"
    in
        Hov.hover
            [ ( "border-color", "#999999" ) ]
            Html.div
            [ Attr.style
                [ ( "width", "1.5rem" )
                , ( "height", "1.5rem" )
                , ( "background-color", color )
                , ( "border-radius", "1rem" )
                , ( "border", "0.25rem solid" )
                , ( "border-color", "white" )
                ]
            , Evt.onClick coords
            ]
            []


drawChild : Set ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> Bounds -> Html ( Int, Int )
drawChild hits selected coords tree =
    Html.div
        [ Attr.style
            [ ( "width", "40%" )
            , ( "flex-grow", "1" )
            ]
        ]
        [ drawTree hits selected coords tree ]


branches : Html a
branches =
    Html.div
        [ Attr.style
            [ ( "width", "100%" )
            , ( "display", "flex" )
            , ( "justify-content", "center" )
            ]
        ]
        [ Html.div
            [ Attr.style
                [ ( "width", "50%" )
                , ( "height", "1rem" )
                , ( "margin-top", "0.2rem" )
                , ( "border", "1px solid black" )
                , ( "border-radius", "0.5rem" )
                , ( "border-bottom", "none" )
                ]
            ]
            []
        ]


float : Float -> String
float x =
    let
        sign =
            if x > -5.0e-3 then
                "+"
            else
                "-"

        cents =
            round (abs x * 100)

        integerPart =
            cents // 100

        centRemainder =
            cents % 100

        decimal =
            if centRemainder < 10 then
                ".0"
            else
                "."
    in
        String.concat
            [ sign, toString integerPart, decimal, toString centRemainder ]
