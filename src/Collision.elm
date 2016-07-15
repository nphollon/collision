module Collision exposing (Body, Bounds, Face, Vector, Quaternion, face, vector, quaternion, axisAngleRotation, collide, create, encode, decode)

{-| Detect collisions between rigid three-dimensional objects. The process goes like this:

1. Start with a set of triangular faces that describe the shape of an object.

2. Use `create` to convert the list of faces into a `Bounds` value. Creating the bounds can be time-consuming, so you will want to do this before you start the time loop.

3. If you want to create your bounds ahead of time, you can `encode` them to JSON and `decode` them later.

4. Once your simulation/game is running, test for collisions using `collide`.

This module will not work for 2D objects.

# Geometry
@docs Vector, vector, Quaternion, quaternion, axisAngleRotation, Face, face

# Creating Bounds
@docs Bounds, create

# Checking Collisions
@docs Body, collide

# Working with JSON
@docs encode, decode
-}

import Json.Encode exposing (Value)
import Json.Decode exposing (Decoder)
import OBBTree
import Face
import Vector
import Quaternion


{-| Stores a three-dimensional position.
-}
type alias Vector =
    Vector.Vector


{-| Create a vector from x, y, and z coordinates.
-}
vector : Float -> Float -> Float -> Vector
vector =
    Vector.vector


{-| Stores a three-dimensional rotation.
-}
type alias Quaternion =
    Quaternion.Quaternion


{-| Create a quaternion from w, x, y, and z coordinates.
-}
quaternion : Float -> Float -> Float -> Float -> Quaternion
quaternion =
    Quaternion.quaternion


{-| Given an axis to rotate around and an angle of rotation, create a quaternion.
-}
axisAngleRotation : Vector -> Float -> Quaternion
axisAngleRotation =
    Quaternion.fromAxisAngle


{-| A triangle. The surface of your colliding objects is described by a collection of triangular faces.
-}
type alias Face =
    Face.Face


{-| Create a triangular face, given the positions of its three vertexes. The vertexes can be given in any order.
-}
face : Vector -> Vector -> Vector -> Face
face =
    Face.face


{-| The boundary data for an object, stored as an OBBTree.
-}
type alias Bounds =
    OBBTree.OBBTree


{-| An object that is positioned and oriented in three-dimensional space. The bounds of the object are given in the body's reference frame. Before testing for a collision, we use the position and orientation to move the bounds into the world's reference frame.

This way, we can move our objects through the world, but we don't have to re-compute the bounds (as long as the object does not change shape).
-}
type alias Body a =
    { a
        | position : Vector
        , orientation : Quaternion
        , bounds : Maybe Bounds
    }


{-| Generate the OBBTree for a surface. The list of faces defines the surface. If the list is empty, the function returns Nothing.

The overall time to build the OBBTree is O(n log^2 n), where n is the number of faces.
-}
create : List Face -> Bounds
create =
    OBBTree.create


{-| Determine whether two bodies collide.
-}
collide : Body b -> Body b' -> Bool
collide =
    OBBTree.collide


{-| Encode an OBBTree as JSON.
-}
encode : Bounds -> Value
encode =
    OBBTree.encode


{-| A JSON decoder for an OBBTree encoded with the above function.
-}
decode : Decoder Bounds
decode =
    OBBTree.decode
