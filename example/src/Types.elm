module Types exposing (..)

import Set exposing (Set)
import Math.Vector3 exposing (Vec3)
import Vector exposing (Vector)
import Quaternion exposing (Quaternion)


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
    | PlacementEditor Solid
    | ShapeEditor
    | ViewEditor
    | Transition TransitionDetails


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
    = ChangeRoom Room
    | BackToEntrance
    | ExtrinsicNudge Vector
    | ExtrinsicRotate Quaternion
    | SetShape Solid String
    | SelectNode Solid ( Int, Int )
    | CollisionsOnly Bool
    | ShowBoxes Bool
    | SetTreeLevel Int
    | Tick Float
