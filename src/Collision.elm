module Collision exposing (Body, Bounds, Vector, Face, collide, create, encode, decode)

{-| Detect collisions between three-dimensional objects.

This module will not work for 2D objects! Even if you add a dummy z-coordinate to your vertex data.

# Geometric Types
@docs Vector, Face

# Creating Bounds
@docs Bounds, create

# Checking Collisions
@docs Body, collide

# Working with JSON
@docs encode, decode
-}

import Json.Encode exposing (Value)
import Json.Decode exposing (Decoder)
import BoundingBox exposing (BoundingBox)
import Tree exposing (Tree)
import Vector
import Face


{-| Three dimensional vector type
-}
type alias Vector =
    Vector.Vector


{-| A face is a triangle. Its vertexes are at points P, Q, and R. Vertexes can be in any order.
-}
type alias Face =
    Face.Face


{-| The boundary data for an object, stored as an OBBTree.
-}
type alias Bounds =
    Tree BoundingBox


{-| An object in three-dimensional space. When testing for collisions, the bounds are repositioned to the given position and orientation.

TODO: A better rotation formalism.
-}
type alias Body a =
    { a
        | position : Vector
        , orientation : Vector
        , bounds : Bounds
    }


{-| Generate the OBBTree for a surface. The list of faces defines the surface.

The overall time to build the OBBTree is O(n log^2 n), where n is the number of faces.

TODO: Get rid of int argument
TODO: Fix behavior for single faces
-}
create : Int -> List Face -> Bounds
create =
    BoundingBox.create


{-| Determine whether two bodies collide.

TODO: Triangle collisions
-}
collide : Body b -> Body b' -> Bool
collide a b =
    BoundingBox.collide a a.bounds b b.bounds


{-| Encode an OBBTree as JSON.
-}
encode : Bounds -> Value
encode =
    Tree.encode BoundingBox.encode


{-| A JSON decoder for an OBBTree encoded with the above function.
-}
decode : Decoder Bounds
decode =
    Tree.decode BoundingBox.decode
