module Controls exposing (draw)

import String
import Json.Decode as Json
import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events as Evt
import InlineHover as Hov
import Ease


-- Collision Library

import Vector exposing (Vector)


-- Project Local

import Types exposing (..)
import Elements


draw : Model -> Html Action
draw model =
    let
        ( title, controls ) =
            roomAppearance model model.room
    in
        Html.div
            [ Attr.style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "width", "250px" )
                ]
            ]
            [ Html.div
                [ Attr.style
                    [ ( "position", "relative" )
                    , ( "overflow", "hidden" )
                    , ( "height", "10%" )
                    ]
                ]
                [ title
                ]
            , Html.div
                [ Attr.style
                    [ ( "position", "relative" )
                    , ( "overflow", "hidden" )
                    , ( "height", "90%" )
                    ]
                ]
                [ Elements.divider
                , controls
                ]
            ]


roomAppearance : Model -> Room -> ( Html Action, Html Action )
roomAppearance model room =
    case room of
        Entrance ->
            ( title "Collision Test", entranceControls )

        PositionEditor _ ->
            ( title "Change Position", positionControls )

        OrientationEditor _ ->
            ( title "Change Orientation", orientationControls )

        ShapeEditor ->
            ( title "Change Shapes", shapeControls model )

        ViewEditor ->
            ( title "View Settings", viewControls model )

        Transition settings ->
            transition settings model


transition : TransitionDetails -> Model -> ( Html Action, Html Action )
transition settings model =
    let
        ( fromTitle, fromContent ) =
            roomAppearance model settings.origin

        ( toTitle, toContent ) =
            roomAppearance model settings.destination

        slide from to =
            if settings.returning then
                Html.div []
                    [ offsetDiv (min 0 (settings.progress - 1)) to
                    , offsetDiv settings.progress from
                    ]
            else
                Html.div []
                    [ offsetDiv (max 0 (1 - settings.progress)) to
                    , offsetDiv -settings.progress from
                    ]
    in
        ( slide fromTitle toTitle, slide fromContent toContent )


offsetDiv : Float -> Html a -> Html a
offsetDiv fraction content =
    let
        percent =
            round (100 * Ease.inOutCubic fraction)

        percentString =
            toString percent ++ "%"
    in
        Html.div
            [ Attr.style
                [ ( "position", "absolute" )
                , ( "margin-left", percentString )
                , ( "width", "100%" )
                ]
            ]
            [ content ]


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
            , button (ChangeRoom ShapeEditor) "Shapes"
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
            , select
                toAxis
                True
                "x"
                [ ( "x", "X Axis" )
                , ( "y", "Y Axis" )
                , ( "z", "Z Axis" )
                ]
            , Elements.spacer
            , button ExtrinsicRotate "Extrinsic Rotate"
            , button IntrinsicRotate "Intrinsic Rotate"
            , button ResetOrientation "Reset"
            ]


shapeControls : Model -> Html Action
shapeControls model =
    submenu
        [ Html.text "Red Shape"
        , select (SetShape Red)
            True
            model.red.shape
            [ ( "cube", "Cube" )
            , ( "icosahedron", "Icosahedron" )
            , ( "ring", "Ring" )
            ]
        , Elements.spacer
        , Html.text "Blue Shape"
        , select (SetShape Blue)
            True
            model.blue.shape
            [ ( "cube", "Cube" )
            , ( "icosahedron", "Icosahedron" )
            , ( "ring", "Ring" )
            ]
        ]


viewControls : Model -> Html Action
viewControls settings =
    let
        treeLevels =
            List.map (\i -> ( toString i, toString i )) [1..5]

        setTreeLevel value =
            String.toInt value
                |> Result.withDefault 1
                |> SetTreeLevel
    in
        submenu
            [ checkbox
                CollisionsOnly
                settings.collisionsOnly
                "Show collisions only"
            , Elements.spacer
            , checkbox
                ShowBoxes
                settings.showBoxes
                "Show bounding boxes"
            , Html.div []
                [ Html.text "Tree level"
                , select
                    setTreeLevel
                    settings.showBoxes
                    (toString settings.treeLevel)
                    treeLevels
                ]
            ]


checkbox : (Bool -> a) -> Bool -> String -> Html a
checkbox sendMsg isChecked label =
    let
        id =
            String.toLower label
                |> String.split " "
                |> String.join "-"
    in
        Html.div
            [ Attr.style [ ( "margin", "5px" ) ] ]
            [ Html.label [ Attr.for id ] [ Html.text label ]
            , Html.input
                [ Attr.type' "checkbox"
                , Attr.id id
                , Attr.checked isChecked
                , Evt.onCheck sendMsg
                ]
                []
            ]


select : (String -> a) -> Bool -> String -> List ( String, String ) -> Html a
select sendMsg isEnabled selection options =
    let
        toOption ( value, label ) =
            Html.option
                [ Attr.selected (selection == value)
                , Attr.value value
                ]
                [ Html.text label ]

        handler =
            Evt.on "change" (Json.map sendMsg Evt.targetValue)
    in
        Html.select
            [ handler
            , Attr.disabled (not isEnabled)
            , Attr.style
                [ ( "width", "5rem" )
                , ( "margin", "0.5rem" )
                ]
            ]
            (List.map toOption options)


backButton : Html Action
backButton =
    button BackToEntrance "Back"


inputField : (String -> msg) -> String -> Html msg
inputField sendMsg label =
    Html.div [ Attr.style [ ( "margin", "0.2rem" ) ] ]
        [ Html.text label
        , Html.input
            [ Attr.style
                [ ( "border", "none" )
                , ( "background-color", "#eeeeee" )
                , ( "padding", "0.3rem" )
                ]
            , Attr.size 3
            , Evt.onInput sendMsg
            ]
            []
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
