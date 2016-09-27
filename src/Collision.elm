module Collision exposing (Body, Bounds, Face, face, collide, create, empty, encode, decode, BoundingBox, collisionMap)

{-| Detect collisions between rigid three-dimensional objects. The process goes like this:

1. Start with a set of triangular faces that describe the shape of an object.

2. Use `create` to convert the list of faces into a `Bounds` value. Creating the bounds can be time-consuming, so you will want to do this before you start the time loop.

3. If you want to create your bounds ahead of time, you can `encode` them to JSON and `decode` them later.

4. Once your simulation/game is running, test for collisions using `collide`.

This module will not work for 2D objects.

# Collision Detection
@docs Face, face, Bounds, create, empty, Body, collide

# Working with JSON
@docs encode, decode

# Debugging

Chances are you won't need to use the functions below. They are used by the Collision Visualizer. You can use them along with the `Collision.Tree` module to examine the collision trees.

@docs BoundingBox, collisionMap
-}

import Set exposing (Set)
import Json.Encode exposing (Value)
import Json.Decode exposing (Decoder)
import Vector exposing (Vector)
import Frame exposing (Frame)
import Collision.OBBTree as OBBTree
import Collision.Face as Face
import Collision.Tree exposing (Tree)


{-| A triangle. The surface of your colliding objects is described by a collection of triangular faces. The `Vector` type is defined in [nphollon/geo3d](package.elm-lang.org/packages/nphollon/geo3d/latest/Vector).
-}
type alias Face =
    { p : Vector
    , q : Vector
    , r : Vector
    }


{-| Create a triangular face, given the positions of its three vertexes. The vertexes can be given in any order.
-}
face : Vector -> Vector -> Vector -> Face
face =
    Face.face


{-| The boundary data for an object, stored as an OBBTree.
-}
type alias Bounds =
    Tree BoundingBox Face


{-| An object that is positioned and oriented in three-dimensional space. The bounds of the object are given in the body's reference frame. The `Frame` type is defined in [nphollon/geo3d](package.elm-lang.org/packages/nphollon/geo3d/latest/Frame).

Before testing for a collision, we use the position and orientation to move the bounds into the world's reference frame. This way, we can move our objects through the world, but we don't have to re-compute the bounds (as long as the object does not change shape).
-}
type alias Body a =
    { a
        | frame : Frame
        , bounds : Bounds
    }


{-| Generate the bounding tree for an object. The list of faces defines the surface.

The overall time to build the OBBTree is O(n log^2 n), where n is the number of faces.
-}
create : List Face -> Bounds
create =
    OBBTree.create


{-| Create an empty bounding tree. This will not collide with anything.
-}
empty : Bounds
empty =
    OBBTree.empty


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


{-| An oriented bounding box. A, B, and C are the radiuses, or half-widths, of the box along its X, Y, and Z axes, respectively. The frame describes the placement of the bounding box relative to the body's reference frame.
-}
type alias BoundingBox =
    { a : Float
    , b : Float
    , c : Float
    , frame : Frame
    }


{-| Given two bodies, A and B, return the set of tree coordinates where the bounding tree of A collides with the bounding tree of B. This gives you an inkling of how the collision algorithm works. In general, boxes that collide with boxes or leaves on the same level are hits, and leaves that collide with leafs are hits.

If A has a bounding box at (1, 2), and it collides with a bounding box in B at (1, 4), then `collisionMap a b` will include (1, 2), and `collisionMap b a` will include (1, 4).

If A has a leaf at (3, 0), and it collides with a leaf in B at (4, 2), then `collisionMap a b` will include (3, 0), and `collisionMap b a` will include (4, 2).

Check out the Collision Visualizer to see this function in action.

For more information about how the tree coordinates work, see `Collision.Tree`.
-}
collisionMap : Body b -> Body b' -> Set ( Int, Int )
collisionMap =
    OBBTree.collisionMap
