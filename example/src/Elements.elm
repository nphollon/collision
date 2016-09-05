module Elements exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr


title : String -> Html a
title label =
    Html.h1
        [ Attr.style
            [ ( "font-size", "1.2rem" )
            , ( "font-weight", "normal" )
            , ( "text-align", "center" )
            ]
        ]
        [ Html.text label ]


divider : Html a
divider =
    Html.hr [ Attr.style [ ( "width", "80%" ) ] ] []


spacer : Html a
spacer =
    Html.div
        [ Attr.style [ ( "width", "100%" ), ( "margin", "10px 0px" ) ] ]
        [ Html.text " " ]
