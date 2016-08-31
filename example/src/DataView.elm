module DataView exposing (draw)

import String
import Html exposing (Html)
import Html.Attributes as Attr
import Html.App as App
import Html.Events as Evt
import InlineHover as Hov


-- Collision Library

import Collision exposing (Body, Bounds, Vector, Quaternion)
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
    in
        Html.div
            [ Attr.style [ ( "width", "750px" ) ] ]
            [ Html.h2 [ titleStyle ] [ Html.text "Red" ]
            , Elements.divider
            , App.map (SelectNode Red) (displayBody model.red)
            , Html.h2 [ titleStyle ] [ Html.text "Blue" ]
            , Elements.divider
            , App.map (SelectNode Blue) (displayBody model.blue)
            ]


displayBody : Entity -> Html ( Int, Int )
displayBody entity =
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
                [ drawTree entity.selectedNode ( 0, 0 ) entity.bounds
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


drawTree : ( Int, Int ) -> ( Int, Int ) -> Bounds -> Html ( Int, Int )
drawTree selected coords tree =
    case tree of
        Leaf b ->
            Html.div
                [ Attr.style
                    [ ( "display", "flex" )
                    , ( "justify-content", "center" )
                    ]
                ]
                [ nodeBox selected coords ]

        Node a left right ->
            Html.div
                [ Attr.style
                    [ ( "display", "flex" )
                    , ( "flex-wrap", "wrap" )
                    , ( "justify-content", "center" )
                    ]
                ]
                [ nodeBox selected coords
                , branches
                , drawChild selected (toTheLeft coords) left
                , drawChild selected (toTheRight coords) right
                ]


toTheLeft : ( Int, Int ) -> ( Int, Int )
toTheLeft ( level, offset ) =
    ( level + 1, 2 * offset )


toTheRight : ( Int, Int ) -> ( Int, Int )
toTheRight ( level, offset ) =
    ( level + 1, 2 * offset + 1 )


nodeBox : ( Int, Int ) -> ( Int, Int ) -> Html ( Int, Int )
nodeBox selected coords =
    let
        color =
            if selected == coords then
                "blue"
            else
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


drawChild : ( Int, Int ) -> ( Int, Int ) -> Bounds -> Html ( Int, Int )
drawChild selected coords tree =
    Html.div
        [ Attr.style
            [ ( "width", "40%" )
            , ( "flex-grow", "1" )
            ]
        ]
        [ drawTree selected coords tree ]


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
