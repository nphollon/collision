module Controls exposing (draw)

import Char
import String
import Json.Decode as Json
import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events as Evt
import InlineHover as Hov
import Ease
import Vector exposing (Vector)
import Quaternion


-- Collision Library

import Collision.Tree as Tree


-- Project Local

import Types exposing (..)
import Elements
import Mesh


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
                [ title ]
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
            ( Elements.title "Collision Test", entranceControls )

        PlacementEditor Red ->
            ( Elements.title "Move Red", placementControls )

        PlacementEditor Blue ->
            ( Elements.title "Move Blue", placementControls )

        ShapeEditor ->
            ( Elements.title "Change Shapes", shapeControls model )

        ViewEditor ->
            ( Elements.title "View Settings", viewControls model )

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


entranceControls : Html Action
entranceControls =
    let
        editPlacementFor =
            PlacementEditor >> ChangeRoom
    in
        Html.div
            [ controlPanelStyle ]
            [ button (editPlacementFor Red) "Move Red"
            , button (editPlacementFor Blue) "Move Blue"
            , button (ChangeRoom ShapeEditor) "Shapes"
            , button (ChangeRoom ViewEditor) "View Settings"
            ]


placementControls : Html Action
placementControls =
    submenu
        [ Html.text "Nudge"
        , nudgeArray "X" (Vector.vector 1 0 0)
        , nudgeArray "Y" (Vector.vector 0 1 0)
        , nudgeArray "Z" (Vector.vector 0 0 -1)
        , Elements.spacer
        , Html.text "Rotate"
        , turnArray "X" (Vector.vector -1 0 0)
        , turnArray "Y" (Vector.vector 0 1 0)
        , turnArray "Z" (Vector.vector 0 0 -1)
        ]


nudgeArray : String -> Vector -> Html Action
nudgeArray name unit =
    let
        nudge x =
            ExtrinsicNudge (Vector.scale x unit)
    in
        Html.span []
            [ smallButton (nudge -1) "«"
            , smallButton (nudge -0.1) "‹"
            , Html.text name
            , smallButton (nudge 0.1) "›"
            , smallButton (nudge 1) "»"
            ]


turnArray : String -> Vector -> Html Action
turnArray name unit =
    let
        turn x =
            { scalar = x / (2 - sqrt 3), vector = unit }
                |> Quaternion.normalize
                |> Maybe.withDefault Quaternion.identity
                |> ExtrinsicRotate
    in
        Html.span []
            [ smallButton (turn -1) "«"
            , smallButton (turn -10) "‹"
            , Html.text name
            , smallButton (turn 10) "›"
            , smallButton (turn 1) "»"
            ]


smallButton : msg -> String -> Html msg
smallButton sendMsg label =
    Hov.hover
        [ ( "background-color", "#eeeeee" ) ]
        Html.button
        [ Evt.onClick sendMsg
        , Attr.style
            [ ( "background-color", "#ffffff" )
            , ( "border", "none" )
            , ( "font-size", "1rem" )
            , ( "padding", "5px" )
            , ( "margin", "5px" )
            ]
        ]
        [ Html.text label ]


shapeControls : Model -> Html Action
shapeControls model =
    let
        shapeOptions =
            List.map
                (\name -> ( name, capitalize name ))
                Mesh.options
    in
        submenu
            [ Html.text "Red Shape"
            , select (SetShape Red) True model.red.shape shapeOptions
            , Elements.spacer
            , Html.text "Blue Shape"
            , select (SetShape Blue) True model.blue.shape shapeOptions
            ]


capitalize : String -> String
capitalize text =
    case String.uncons text of
        Just ( head, tail ) ->
            String.cons (Char.toUpper head) tail

        Nothing ->
            text


viewControls : Model -> Html Action
viewControls model =
    let
        maxDepth =
            max (Tree.depth model.red.bounds)
                (Tree.depth model.blue.bounds)

        treeLevels =
            List.map (\i -> ( toString i, toString i )) (List.range 1 maxDepth)

        setTreeLevel value =
            String.toInt value
                |> Result.withDefault 1
                |> SetTreeLevel
    in
        submenu
            [ checkbox
                CollisionsOnly
                model.collisionsOnly
                "Show collisions only"
            , Elements.spacer
            , checkbox
                ShowBoxes
                model.showBoxes
                "Show bounding boxes"
            , Html.div []
                [ Html.text "Tree level"
                , select
                    setTreeLevel
                    model.showBoxes
                    (toString model.treeLevel)
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
                [ Attr.type_ "checkbox"
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
