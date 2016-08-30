module DataView exposing (draw)

import String
import Html exposing (Html)
import Html.Attributes as Attr


-- Collision Library

import Collision exposing (Body)


-- Project Local

import Types exposing (..)
import Elements


draw : Model -> Html a
draw model =
    let
        titleStyle =
            Attr.style
                [ ( "font-size", "1.2rem" )
                , ( "font-weight", "normal" )
                , ( "text-align", "center" )
                ]
    in
        Html.div [ Attr.style [ ( "width", "250px" ) ] ]
            [ Html.h2 [ titleStyle ] [ Html.text "Red" ]
            , Elements.divider
            , displayBody model.red
            , Html.h2 [ titleStyle ] [ Html.text "Blue" ]
            , Elements.divider
            , displayBody model.blue
            ]


displayBody : Body b -> Html a
displayBody { frame } =
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
