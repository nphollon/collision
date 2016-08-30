module Main exposing (main)

import String
import Html exposing (Html)
import Html.Attributes as Attr
import Html.App as App


-- Collision Library

import Collision
import Vector exposing (Vector)
import Quaternion exposing (Quaternion)
import Frame exposing (Frame)


-- Project Local

import Types exposing (..)
import Controls
import OrthoView
import DataView


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
    , redFrame = Frame.identity
    , blueFrame =
        { position = Vector.vector 0 0 -5
        , orientation = Quaternion.identity
        }
    , redBounds = Collision.empty
    , blueBounds = Collision.empty
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

        _ ->
            model


type alias WithSolid a =
    { a | solid : Solid }


updateSolid : (WithSolid a -> Frame -> Frame) -> WithSolid a -> Model -> Model
updateSolid transform fields model =
    case fields.solid of
        Red ->
            { model | redFrame = transform fields model.redFrame }

        Blue ->
            { model | blueFrame = transform fields model.blueFrame }


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
