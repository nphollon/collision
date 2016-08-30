module Types exposing (..)

import Vector exposing (Vector)
import Frame exposing (Frame)
import Collision exposing (Bounds)


type alias Model =
    { room : Room
    , redFrame : Frame
    , redBounds : Bounds
    , blueFrame : Frame
    , blueBounds : Bounds
    }


type Room
    = Entrance
    | PositionEditor PositionFields
    | OrientationEditor OrientationFields


type alias PositionFields =
    { xText : String
    , yText : String
    , zText : String
    , solid : Solid
    }


type alias OrientationFields =
    { angleText : String
    , axis : Vector
    , solid : Solid
    }


type Solid
    = Red
    | Blue


type Action
    = EditX String
    | EditY String
    | EditZ String
    | EditAngle String
    | SetAxis Vector
    | ChangeRoom Room
    | SetPosition
    | ExtrinsicNudge
    | IntrinsicNudge
    | ExtrinsicRotate
    | IntrinsicRotate
    | ResetOrientation
