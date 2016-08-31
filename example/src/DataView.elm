module DataView exposing (draw)

import String
import Html exposing (Html)
import Html.Attributes as Attr
import Html.App as App
import Html.Events as Evt
import InlineHover as Hov


-- Collision Library

import Collision exposing (Body, Bounds)
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
        Html.div [ Attr.style [ ( "width", "750px" ) ] ]
            [ Html.h2 [ titleStyle ] [ Html.text "Red" ]
            , Elements.divider
            , App.map (SelectNode Red) (displayBody model.red)
            , Html.h2 [ titleStyle ] [ Html.text "Blue" ]
            , Elements.divider
            , App.map (SelectNode Blue) (displayBody model.blue)
            ]


displayBody : Body b -> Html ( Int, Int )
displayBody { frame, bounds } =
    Html.div
        [ Attr.style
            [ ( "display", "flex" )
            , ( "flex-wrap", "wrap" )
            , ( "justify-content", "space-around" )
            , ( "margin-bottom", "25px" )
            ]
        ]
        [ Html.div []
            [ Html.text ("X = " ++ float frame.position.x)
            , Html.br [] []
            , Html.text ("Y = " ++ float frame.position.y)
            , Html.br [] []
            , Html.text ("Z = " ++ float frame.position.z)
            ]
        , Html.div []
            [ Html.text ("Qw = " ++ float frame.orientation.scalar)
            , Html.br [] []
            , Html.text ("Qx = " ++ float frame.orientation.vector.x)
            , Html.br [] []
            , Html.text ("Qy = " ++ float frame.orientation.vector.y)
            , Html.br [] []
            , Html.text ("Qz = " ++ float frame.orientation.vector.z)
            ]
        , Html.div [ Attr.style [ ( "width", "100%" ) ] ]
            [ drawTree 0 0 bounds
            ]
        ]


drawTree : Int -> Int -> Bounds -> Html ( Int, Int )
drawTree level offset tree =
    case tree of
        Leaf b ->
            Html.div
                [ Attr.style
                    [ ( "display", "flex" )
                    , ( "justify-content", "center" )
                    ]
                ]
                [ nodeBox level offset ]

        Node a left right ->
            Html.div
                [ Attr.style
                    [ ( "display", "flex" )
                    , ( "flex-wrap", "wrap" )
                    , ( "justify-content", "center" )
                    ]
                ]
                [ nodeBox level offset
                , branches
                , drawChild (1 + level) (2 * offset) left
                , drawChild (1 + level) (2 * offset + 1) right
                ]


nodeBox : Int -> Int -> Html ( Int, Int )
nodeBox level offset =
    Hov.hover
        [ ( "background-color", "#9999dd" ) ]
        Html.div
        [ Attr.style
            [ ( "width", "1.5rem" )
            , ( "height", "1.5rem" )
            , ( "background-color", "black" )
            , ( "border-radius", "0.75rem" )
            ]
        , Evt.onClick ( level, offset )
        ]
        []


drawChild : Int -> Int -> Bounds -> Html ( Int, Int )
drawChild level offset tree =
    Html.div
        [ Attr.style
            [ ( "width", "40%" )
            , ( "flex-grow", "1" )
            ]
        ]
        [ drawTree level offset tree ]


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
