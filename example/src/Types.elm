module Types exposing (..)

import Set exposing (Set)
import Math.Vector3 exposing (Vec3)
import Vector exposing (Vector)


-- Collision Library

import Collision exposing (Bounds, Body)


type alias Model =
    { room : Room
    , red : Entity
    , blue : Entity
    , collision : Bool
    , collisionsOnly : Bool
    , showBoxes : Bool
    , treeLevel : Int
    }


type alias Entity =
    Body
        { selectedNode : ( Int, Int )
        , hits : Set ( Int, Int )
        , shape : String
        }


type Room
    = Entrance
    | PositionEditor PositionFields
    | OrientationEditor OrientationFields
    | ShapeEditor
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
    , returning : Bool
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
    | BackToEntrance
    | SetPosition
    | ExtrinsicNudge
    | ExtrinsicRotate
    | ResetOrientation
    | SetShape Solid String
    | SelectNode Solid ( Int, Int )
    | CollisionsOnly Bool
    | ShowBoxes Bool
    | SetTreeLevel Int
    | Tick Float
