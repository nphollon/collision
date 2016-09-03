module Types exposing (..)

import Set exposing (Set)
import Math.Vector3 exposing (Vec3)


-- Collision Library

import Vector exposing (Vector)
import Collision exposing (Bounds, Body)


type alias Model =
    { room : Room
    , red : Entity
    , blue : Entity
    , collisionsOnly : Bool
    , showBoxes : Bool
    , treeLevel : Int
    }


type alias Entity =
    Body
        { selectedNode : ( Int, Int )
        , hits : Set ( Int, Int )
        }


type Room
    = Entrance
    | PositionEditor PositionFields
    | OrientationEditor OrientationFields
    | ViewEditor
    | Transition TransitionDetails


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


type alias TransitionDetails =
    { origin : Room
    , destination : Room
    , progress : Float
    }


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
    | CollisionsOnly Bool
    | ShowBoxes Bool
    | SetTreeLevel Int
    | Tick Float
