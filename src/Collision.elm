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
import OBBTree
import Face exposing (Face)


{-| The boundary data for an object, stored as an OBBTree.
-}
type alias Bounds =
    OBBTree.OBBTree


{-| An object in three-dimensional space. When testing for collisions, the bounds are repositioned to the given position and orientation.
-}
type alias Body a =
    OBBTree.Body a


{-| Generate the OBBTree for a surface. The list of faces defines the surface. If the list is empty, the function returns Nothing.

The overall time to build the OBBTree is O(n log^2 n), where n is the number of faces.

Bug: Failing to partition faces
Bug: Box-face collision only works one way
-}
create : List Face -> Maybe Bounds
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
