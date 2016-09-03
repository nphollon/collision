module Main exposing (main)

import Array
import Set
import String
import Time
import Html exposing (Html)
import Html.Attributes as Attr
import Html.App as App
import AnimationFrame


-- Collision Library

import Collision
import Face exposing (Face)
import Frame exposing (Frame)
import OBBTree
import Quaternion exposing (Quaternion)
import Vector exposing (Vector)


-- Project Local

import Types exposing (..)
import Controls
import OrthoView
import DataView
import Model


main : Program Never
main =
    App.program
        { init = init ! []
        , update = \action model -> update action model ! []
        , subscriptions = subscriptions
        , view = view
        }


init : Model
init =
    updateCollisionMap
        { room =
            Easing
                { origin = Entrance
                , destination =
                    PositionEditor
                        { xText = ""
                        , yText = ""
                        , zText = ""
                        , solid = Red
                        }
                , time = 0.8
                }
        , red =
            { frame = Frame.identity
            , bounds = Collision.create cube
            , hits = Set.empty
            , selectedNode = ( 0, 0 )
            }
        , blue =
            { frame =
                { position = Vector.vector 0 1.6 -2
                , orientation =
                    Quaternion.quaternion 0.85 0.35 0.35 0.15
                        |> Quaternion.scale (1.010101)
                }
            , bounds = Collision.create cube
            , hits = Set.empty
            , selectedNode = ( 0, 0 )
            }
        , collisionsOnly = False
        , showBoxes = False
        , treeLevel = 1
        }


cube : List Face
cube =
    Model.toFaces
        { vertexPositions =
            Array.fromList
                [ Vector.vector -1 1 1
                , Vector.vector 1 1 1
                , Vector.vector 1 -1 1
                , Vector.vector -1 -1 1
                , Vector.vector -1 1 -1
                , Vector.vector 1 1 -1
                , Vector.vector 1 -1 -1
                , Vector.vector -1 -1 -1
                ]
        , vertexIndexes =
            [ [ 3, 2, 1, 0 ]
            , [ 5, 4, 0, 1 ]
            , [ 6, 5, 1, 2 ]
            , [ 7, 6, 2, 3 ]
            , [ 7, 3, 0, 4 ]
            , [ 7, 4, 5, 6 ]
            ]
        }


subscriptions : Model -> Sub Action
subscriptions model =
    case model.room of
        Easing _ ->
            AnimationFrame.diffs (Time.inSeconds >> Tick)

        _ ->
            Sub.none


update : Action -> Model -> Model
update action model =
    case ( action, model.room ) of
        --
        -- Navigate the UI
        --
        ( Tick dt, Easing easing ) ->
            if easing.time >= 1 then
                { model | room = easing.destination }
            else
                { model
                    | room =
                        Easing { easing | time = easing.time + dt }
                }

        ( ChangeRoom room, _ ) ->
            { model
                | room =
                    Easing
                        { origin = model.room
                        , destination = room
                        , time = 0
                        }
            }

        ( EditX xText, PositionEditor fields ) ->
            { model | room = PositionEditor { fields | xText = xText } }

        ( EditY yText, PositionEditor fields ) ->
            { model | room = PositionEditor { fields | yText = yText } }

        ( EditZ zText, PositionEditor fields ) ->
            { model | room = PositionEditor { fields | zText = zText } }

        ( EditAngle angleText, OrientationEditor fields ) ->
            { model
                | room =
                    OrientationEditor { fields | angleText = angleText }
            }

        ( SetAxis axis, OrientationEditor fields ) ->
            { model
                | room = OrientationEditor { fields | axis = axis }
            }

        --
        -- Move the entities
        --
        ( SetPosition, PositionEditor fields ) ->
            updateFrame
                (parseVector >> Frame.setPosition)
                fields
                model

        ( ExtrinsicNudge, PositionEditor fields ) ->
            updateFrame
                (parseVector >> Frame.extrinsicNudge)
                fields
                model

        ( IntrinsicNudge, PositionEditor fields ) ->
            updateFrame
                (parseVector >> Frame.intrinsicNudge)
                fields
                model

        ( ExtrinsicRotate, OrientationEditor fields ) ->
            updateFrame
                (parseRotation >> Frame.extrinsicRotate)
                fields
                model

        ( IntrinsicRotate, OrientationEditor fields ) ->
            updateFrame
                (parseRotation >> Frame.intrinsicRotate)
                fields
                model

        ( ResetOrientation, OrientationEditor fields ) ->
            updateFrame
                (\_ -> Frame.setOrientation Quaternion.identity)
                fields
                model

        --
        -- Change what information is displayed
        --
        ( SelectNode solid coords, _ ) ->
            updateEntity
                (\body -> { body | selectedNode = coords })
                solid
                model

        ( CollisionsOnly isChecked, _ ) ->
            { model | collisionsOnly = isChecked }

        ( ShowBoxes isChecked, _ ) ->
            { model | showBoxes = isChecked }

        ( SetTreeLevel treeLevel, _ ) ->
            { model | treeLevel = treeLevel }

        _ ->
            model


type alias WithSolid a =
    { a | solid : Solid }


updateFrame : (WithSolid a -> Frame -> Frame) -> WithSolid a -> Model -> Model
updateFrame transform fields model =
    let
        reframe body =
            { body | frame = transform fields body.frame }
    in
        updateEntity reframe fields.solid model
            |> updateCollisionMap


updateEntity : (Entity -> Entity) -> Solid -> Model -> Model
updateEntity transform solid model =
    case solid of
        Red ->
            { model | red = transform model.red }

        Blue ->
            { model | blue = transform model.blue }


updateCollisionMap : Model -> Model
updateCollisionMap model =
    let
        remap =
            (\object other ->
                { object | hits = OBBTree.collisionMap object other }
            )
    in
        { model
            | red = remap model.red model.blue
            , blue = remap model.blue model.red
        }


parseVector : PositionFields -> Vector
parseVector fields =
    Vector.vector
        (toFloat fields.xText)
        (toFloat fields.yText)
        (toFloat fields.zText)


parseRotation : OrientationFields -> Quaternion
parseRotation fields =
    degrees (toFloat fields.angleText)
        |> Quaternion.fromAxisAngle fields.axis
        |> Maybe.withDefault Quaternion.identity


toFloat : String -> Float
toFloat text =
    String.toFloat text
        |> Result.withDefault 0


view : Model -> Html Action
view model =
    Html.div
        [ Attr.style
            [ ( "display", "flex" )
            , ( "flex-wrap", "wrap" )
            , ( "justify-content", "center" )
            ]
        ]
        [ Controls.draw model
        , OrthoView.draw model
        , DataView.draw model
        ]
