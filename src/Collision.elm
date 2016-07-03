module Collision exposing (Body, Bounds, collide, create, encode, decode)

{-| Detect collisions between three-dimensional objects.

This module will not work for 2D objects! Even if you add a dummy z-coordinate to your vertex data.

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
import Vector exposing (Vector)
import Quaternion exposing (Quaternion)
import Face exposing (Face)


{-| The boundary data for an object, stored as an OBBTree.
-}
type alias Bounds =
    Tree BoundingBox BoundingBox


{-| An object in three-dimensional space. When testing for collisions, the bounds are repositioned to the given position and orientation.
-}
type alias Body a =
    { a
        | position : Vector
        , orientation : Quaternion
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
-}
collide : Body b -> Body b' -> Bool
collide a b =
    BoundingBox.collide a a.bounds b b.bounds


{-| Encode an OBBTree as JSON.
-}
encode : Bounds -> Value
encode =
    Tree.encode BoundingBox.encode BoundingBox.encode


{-| A JSON decoder for an OBBTree encoded with the above function.
-}
decode : Decoder Bounds
decode =
    Tree.decode BoundingBox.decode BoundingBox.decode
