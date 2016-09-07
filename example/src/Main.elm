module Main exposing (main)

import Set
import String
import Time exposing (Time)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.App as App
import AnimationFrame


-- Collision Library

import Collision
import Frame exposing (Frame)
import OBBTree
import Quaternion exposing (Quaternion)
import Vector exposing (Vector)


-- Project Local

import Types exposing (..)
import Controls
import OrthoView
import DataView
import Mesh


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
    let
        shape =
            List.head Mesh.options
                |> Maybe.withDefault ""
    in
        updateCollisionMap
            { room = Entrance
            , red =
                { frame = Frame.identity
                , bounds = Mesh.byName shape
                , hits = Set.empty
                , selectedNode = ( 0, 0 )
                , shape = shape
                }
            , blue =
                { frame =
                    { position = Vector.vector 0 1.6 -2
                    , orientation =
                        Quaternion.quaternion 0.85 0.35 0.35 0.15
                    }
                , bounds = Mesh.byName shape
                , hits = Set.empty
                , selectedNode = ( 0, 0 )
                , shape = shape
                }
            , collision = False
            , collisionsOnly = False
            , showBoxes = False
            , treeLevel = 1
            }


subscriptions : Model -> Sub Action
subscriptions model =
    case model.room of
        Transition _ ->
            AnimationFrame.diffs animationFraction

        _ ->
            Sub.none


animationFraction : Time -> Action
animationFraction dt =
    Tick (Time.inMilliseconds dt / Time.inMilliseconds 350)


update : Action -> Model -> Model
update action model =
    case ( action, model.room ) of
        --
        -- Navigate the UI
        --
        ( Tick dt, Transition easing ) ->
            { model | room = tick dt easing }

        ( ChangeRoom room, _ ) ->
            beginTransition False room model

        ( BackToEntrance, _ ) ->
            beginTransition True Entrance model

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

        ( SetShape solid newShape, ShapeEditor ) ->
            updateEntity
                (\body ->
                    { body
                        | shape = newShape
                        , bounds = Mesh.byName newShape
                    }
                )
                solid
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
    updateEntity
        (\body -> { body | frame = transform fields body.frame })
        fields.solid
        model


updateEntity : (Entity -> Entity) -> Solid -> Model -> Model
updateEntity transform solid model =
    case solid of
        Red ->
            updateCollisionMap
                { model | red = transform model.red }

        Blue ->
            updateCollisionMap
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
            , collision = Collision.collide model.red model.blue
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


beginTransition : Bool -> Room -> Model -> Model
beginTransition returning newRoom model =
    { model
        | room =
            Transition
                { origin = model.room
                , destination = newRoom
                , progress = 0
                , returning = returning
                }
    }


tick : Float -> TransitionDetails -> Room
tick dt easing =
    if easing.progress < 1 then
        Transition { easing | progress = easing.progress + dt }
    else
        easing.destination


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
