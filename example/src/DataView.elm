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
import Frame exposing (Frame)
import Face


-- Project Local

import Types exposing (..)
import Elements


draw : Model -> Html Action
draw model =
    let
        redHits =
            OBBTree.collisionMap model.red model.blue

        blueHits =
            OBBTree.collisionMap model.blue model.red
    in
        Html.div
            [ Attr.style [ ( "width", "100%" ) ] ]
            [ Elements.title "Red"
            , Elements.divider
            , App.map (SelectNode Red) (displayBody model.red)
            , Elements.title "Blue"
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
                , ( "flex-direction", "column" )
                , ( "align-items", "center" )
                ]
            ]
            [ Html.div
                [ Attr.style
                    [ ( "line-height", "2rem" )
                    , ( "width", "100%" )
                    ]
                ]
                [ bodyFrameDetails entity.frame
                , Elements.spacer
                , selectedNodeDetails entity.frame selectedSubtree
                ]
            , Html.div
                [ Attr.style
                    [ ( "margin-top", "1rem" )
                    , ( "margin-bottom", "2rem" )
                    ]
                ]
                [ drawTree
                    entity.hits
                    entity.selectedNode
                    ( 0, 0 )
                    entity.bounds
                ]
            ]


bodyFrameDetails : Frame -> Html a
bodyFrameDetails frame =
    section "Model"
        [ vectorDetails "Position" frame.position
        , quaternionDetails "Orientation" frame.orientation
        ]


selectedNodeDetails : Frame -> Bounds -> Html a
selectedNodeDetails bodyFrame tree =
    case tree of
        Leaf face ->
            let
                worldFace =
                    Face.transformOutOf bodyFrame face
            in
                section "Triangle PQR"
                    [ vectorDetails "Local P" face.p
                    , vectorDetails "Local Q" face.q
                    , vectorDetails "Local R" face.r
                    , vectorDetails "Global P" worldFace.p
                    , vectorDetails "Global Q" worldFace.q
                    , vectorDetails "Global R" worldFace.r
                    ]

        Node box _ _ ->
            let
                worldFrame =
                    Frame.compose bodyFrame box.frame
            in
                section "Bounding Box"
                    [ line "Radius"
                        [ ( "A", box.a ), ( "B", box.b ), ( "C", box.c ) ]
                    , vectorDetails "Local Position"
                        box.frame.position
                    , quaternionDetails "Local Orientation"
                        box.frame.orientation
                    , vectorDetails "Global Position"
                        worldFrame.position
                    , quaternionDetails "Global Orientation"
                        worldFrame.orientation
                    ]


section : String -> List (Html a) -> Html a
section name content =
    Html.div
        [ Attr.style
            [ ( "display", "flex" )
            , ( "width", "100%" )
            , ( "justify-content", "center" )
            ]
        ]
        [ Html.div
            [ Attr.style
                [ ( "margin-right", "3rem" )
                ]
            ]
            [ Elements.title name ]
        , Html.div
            [ Attr.style [] ]
            content
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
            [ Attr.style [ ( "display", "flex" ) ] ]
            (Html.em [] [ Html.text label ] :: fields)


field : String -> Float -> Html a
field label value =
    Html.div
        [ Attr.style [ ( "margin-left", "2rem" ) ] ]
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
