module Controls exposing (draw)

import Json.Decode as Json
import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events as Evt
import InlineHover as Hov


-- Collision Library

import Vector exposing (Vector)


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

        ( title, controls ) =
            case model.room of
                Entrance ->
                    ( "Collision Test", entranceControls )

                PositionEditor _ ->
                    ( "Change Position", positionControls )

                OrientationEditor _ ->
                    ( "Change Orientation", orientationControls )

                ViewEditor ->
                    ( "View Settings", viewControls )
    in
        Html.div
            [ Attr.style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "width", "250px" )
                ]
            ]
            [ Html.h1 [ titleStyle ] [ Html.text title ]
            , Elements.divider
            , controls
            ]


entranceControls : Html Action
entranceControls =
    let
        editPositionFor solid =
            { xText = ""
            , yText = ""
            , zText = ""
            , solid = solid
            }
                |> PositionEditor
                |> ChangeRoom

        editOrientationFor solid =
            { angleText = ""
            , axis = Vector.xAxis
            , solid = solid
            }
                |> OrientationEditor
                |> ChangeRoom
    in
        Html.div
            [ controlPanelStyle ]
            [ button (editPositionFor Red) "Red Position"
            , button (editOrientationFor Red) "Red Orientation"
            , Elements.spacer
            , button (editPositionFor Blue) "Blue Position"
            , button (editOrientationFor Blue) "Blue Orientation"
            , Elements.spacer
            , button (ChangeRoom ViewEditor) "View Settings"
            ]


positionControls : Html Action
positionControls =
    submenu
        [ inputField EditX "X "
        , inputField EditY "Y "
        , inputField EditZ "Z "
        , Elements.spacer
        , button SetPosition "Set Position"
        , button ExtrinsicNudge "Extrinsic Nudge"
        , button IntrinsicNudge "Intrinsic Nudge"
        ]


orientationControls : Html Action
orientationControls =
    let
        toAxis value =
            if value == "y" then
                SetAxis Vector.yAxis
            else if value == "z" then
                SetAxis Vector.zAxis
            else
                SetAxis Vector.xAxis
    in
        submenu
            [ inputField EditAngle "Angle (Degrees) "
            , Html.select [ Evt.on "change" (Json.map toAxis Evt.targetValue) ]
                [ Html.option [ Attr.value "x" ] [ Html.text "X Axis" ]
                , Html.option [ Attr.value "y" ] [ Html.text "Y Axis" ]
                , Html.option [ Attr.value "z" ] [ Html.text "Z Axis" ]
                ]
            , Elements.spacer
            , button ExtrinsicRotate "Extrinsic Rotate"
            , button IntrinsicRotate "Intrinsic Rotate"
            , button ResetOrientation "Reset"
            ]


viewControls : Html Action
viewControls =
    submenu
        [ Html.div []
            [ Html.input [ Attr.type' "checkbox" ] []
            , Html.text "Show collisions only"
            ]
        , Html.div []
            [ Html.input [ Attr.type' "checkbox" ] []
            , Html.text "Show bounding boxes"
            ]
        , Html.div []
            [ Html.text "Tree level"
            , Html.select []
                [ Html.option [] [ Html.text "1" ]
                , Html.option [] [ Html.text "2" ]
                , Html.option [] [ Html.text "3" ]
                , Html.option [] [ Html.text "4" ]
                , Html.option [] [ Html.text "5" ]
                ]
            ]
        ]


backButton : Html Action
backButton =
    button (ChangeRoom Entrance) "Back"


inputField : (String -> msg) -> String -> Html msg
inputField sendMsg label =
    Html.div []
        [ Html.text label
        , Html.input [ Attr.size 3, Evt.onInput sendMsg ] []
        ]


button : msg -> String -> Html msg
button sendMsg label =
    Hov.hover
        [ ( "background-color", "#eeeeee" ) ]
        Html.button
        [ Evt.onClick sendMsg
        , Attr.style
            [ ( "background-color", "#ffffff" )
            , ( "border", "none" )
            , ( "width", "80%" )
            , ( "font-size", "1rem" )
            , ( "padding", "5px 10px" )
            , ( "margin", "5px 0px" )
            ]
        ]
        [ Html.text label ]


controlPanelStyle : Attribute Action
controlPanelStyle =
    Attr.style
        [ ( "display", "flex" )
        , ( "flex-direction", "column" )
        , ( "align-items", "center" )
        ]


submenu : List (Html Action) -> Html Action
submenu content =
    let
        fullContent =
            Elements.spacer
                :: content
                ++ [ Elements.spacer, backButton ]
    in
        Html.div [ controlPanelStyle ]
            fullContent
