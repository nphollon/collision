module Elements exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr


divider : Html a
divider =
    Html.hr [ Attr.style [ ( "width", "80%" ) ] ] []


spacer : Html a
spacer =
    Html.div
        [ Attr.style [ ( "width", "100%" ), ( "margin", "10px 0px" ) ] ]
        [ Html.text " " ]
