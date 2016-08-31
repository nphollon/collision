module Types exposing (..)

import WebGL exposing (Drawable)
import Math.Vector3 exposing (Vec3)


-- Collision Library

import Vector exposing (Vector)
import Collision exposing (Bounds, Body)


type alias Model =
    { room : Room
    , red : Entity
    , blue : Entity
    }


type alias Entity =
    Body
        { mesh : Drawable Vertex
        , selectedNode : ( Int, Int )
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


type alias Vertex =
    { position : Vec3
    , normal : Vec3
    }


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
    | SelectNode Solid ( Int, Int )
