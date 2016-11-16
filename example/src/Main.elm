module Main exposing (main)

import Set
import Time exposing (Time)
import Html exposing (Html)
import Html.Attributes as Attr
import AnimationFrame
import Frame exposing (Frame)
import Quaternion exposing (Quaternion)
import Vector exposing (Vector)


-- Collision Library

import Collision


-- Project Local

import Types exposing (..)
import Controls
import OrthoView
import DataView
import Mesh


main : Program Never Model Action
main =
    Html.program
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

        --
        -- Move the entities
        --
        ( ExtrinsicNudge v, PlacementEditor solid ) ->
            updateFrame (Frame.extrinsicNudge v) solid model

        ( ExtrinsicRotate q, PlacementEditor solid ) ->
            updateFrame (Frame.extrinsicRotate q) solid model

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


updateFrame : (Frame -> Frame) -> Solid -> Model -> Model
updateFrame transform solid model =
    updateEntity
        (\body -> { body | frame = transform body.frame })
        solid
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
                { object | hits = Collision.collisionMap object other }
            )
    in
        { model
            | red = remap model.red model.blue
            , blue = remap model.blue model.red
            , collision = Collision.collide model.red model.blue
        }


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
