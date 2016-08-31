module Main exposing (main)

import Array
import String
import Html exposing (Html)
import Html.Attributes as Attr
import Html.App as App


-- Collision Library

import Collision
import Vector exposing (Vector)
import Quaternion exposing (Quaternion)
import Frame exposing (Frame)
import Face exposing (Face)


-- Project Local

import Types exposing (..)
import Controls
import OrthoView
import DataView
import Model


main : Program Never
main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }


init : Model
init =
    { room = Entrance
    , red =
        { frame = Frame.identity
        , bounds = Collision.create cube
        , mesh = Model.drawable cube
        }
    , blue =
        { frame =
            { position = Vector.vector 0 0 -5
            , orientation = Quaternion.identity
            }
        , bounds = Collision.create cube
        , mesh = Model.drawable cube
        }
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


update : Action -> Model -> Model
update action model =
    case ( action, model.room ) of
        ( ChangeRoom room, _ ) ->
            { model | room = room }

        ( EditX xText, PositionEditor fields ) ->
            { model | room = PositionEditor { fields | xText = xText } }

        ( EditY yText, PositionEditor fields ) ->
            { model | room = PositionEditor { fields | yText = yText } }

        ( EditZ zText, PositionEditor fields ) ->
            { model | room = PositionEditor { fields | zText = zText } }

        ( EditAngle angleText, OrientationEditor fields ) ->
            { model | room = OrientationEditor { fields | angleText = angleText } }

        ( SetAxis axis, OrientationEditor fields ) ->
            { model | room = OrientationEditor { fields | axis = axis } }

        ( SetPosition, PositionEditor fields ) ->
            updateSolid (parseVector >> Frame.setPosition) fields model

        ( ExtrinsicNudge, PositionEditor fields ) ->
            updateSolid (parseVector >> Frame.extrinsicNudge) fields model

        ( IntrinsicNudge, PositionEditor fields ) ->
            updateSolid (parseVector >> Frame.intrinsicNudge) fields model

        ( ExtrinsicRotate, OrientationEditor fields ) ->
            updateSolid (parseRotation >> Frame.extrinsicRotate) fields model

        ( IntrinsicRotate, OrientationEditor fields ) ->
            updateSolid (parseRotation >> Frame.intrinsicRotate) fields model

        ( ResetOrientation, OrientationEditor fields ) ->
            updateSolid (\_ -> Frame.setOrientation Quaternion.identity) fields model

        ( SelectNode solid coords, _ ) ->
            let
                _ =
                    Debug.log "Selected" ( solid, coords )
            in
                model

        _ ->
            model


type alias WithSolid a =
    { a | solid : Solid }


updateSolid : (WithSolid a -> Frame -> Frame) -> WithSolid a -> Model -> Model
updateSolid transform fields model =
    let
        updateBody body =
            { body | frame = transform fields body.frame }
    in
        case fields.solid of
            Red ->
                { model | red = updateBody model.red }

            Blue ->
                { model | blue = updateBody model.blue }


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
