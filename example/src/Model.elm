module Model exposing (toFaces, drawable, boxMesh, boxMeshWithWhitelist, MeshData)

import Array exposing (Array)
import Set exposing (Set)
import Maybe.Extra as MaybeX
import Math.Vector3 as Vec3 exposing (Vec3)
import WebGL exposing (Drawable(..))


-- Collision Library

import Vector exposing (Vector)
import Face exposing (Face)
import Collision exposing (Bounds)
import BoundingBox exposing (BoundingBox)
import Tree
import Frame


-- Project local

import Types exposing (..)


type alias MeshData =
    { vertexPositions : Array Vector
    , vertexIndexes : List (List Int)
    }


boxMesh : Int -> Bounds -> Drawable Vertex
boxMesh depth tree =
    let
        keepLeaf ( ( level, _ ), _ ) =
            level < depth

        keepInternal ( ( level, _ ), _ ) =
            level == depth - 1
    in
        boxMeshWithFilters keepLeaf keepInternal tree


boxMeshWithWhitelist : Set ( Int, Int ) -> Int -> Bounds -> Drawable Vertex
boxMeshWithWhitelist whitelist depth tree =
    let
        keepLeaf ( ( level, offset ), _ ) =
            (level < depth)
                && Set.member ( level, offset ) whitelist

        keepInternal ( ( level, offset ), _ ) =
            (level == depth - 1)
                && Set.member ( level, offset ) whitelist
    in
        boxMeshWithFilters keepLeaf keepInternal tree


type alias Indexed a =
    ( ( Int, Int ), a )


boxMeshWithFilters : (Indexed Face -> Bool) -> (Indexed BoundingBox -> Bool) -> Bounds -> Drawable Vertex
boxMeshWithFilters keepLeaf keepInternal tree =
    let
        leaves =
            Tree.leaves tree
                |> List.filter keepLeaf
                |> List.map snd

        internals =
            Tree.internals tree
                |> List.filter keepInternal
                |> List.map snd
                |> List.concatMap boxFaces
    in
        drawable (leaves ++ internals)


drawable : List Face -> Drawable Vertex
drawable =
    List.filterMap toVertexTriangle >> Triangle


toFaces : MeshData -> List Face
toFaces { vertexPositions, vertexIndexes } =
    let
        lookup =
            MaybeX.traverse (flip Array.get vertexPositions)

        decomposePolygon points =
            case points of
                i :: (j :: (k :: list)) ->
                    List.map2 (Face.face i)
                        (j :: k :: list)
                        (k :: list)

                otherwise ->
                    []
    in
        List.filterMap lookup vertexIndexes
            |> List.concatMap decomposePolygon


toVertexTriangle : Face -> Maybe ( Vertex, Vertex, Vertex )
toVertexTriangle face =
    case Vector.normalize (Face.cross face) of
        Nothing ->
            Nothing

        Just normal ->
            Just
                ( toVertex face.p normal
                , toVertex face.q normal
                , toVertex face.r normal
                )


toVertex : Vector -> Vector -> Vertex
toVertex position normal =
    { position = Vec3.fromRecord position
    , normal = Vec3.fromRecord normal
    }


boxFaces : BoundingBox -> List Face
boxFaces box =
    let
        vector signX signY signZ =
            Vector.vector (signX * box.a) (signY * box.b) (signZ * box.c)
                |> Frame.transformOutOf box.frame
    in
        toFaces
            { vertexPositions =
                Array.fromList
                    [ vector -1 1 1
                    , vector 1 1 1
                    , vector 1 -1 1
                    , vector -1 -1 1
                    , vector -1 1 -1
                    , vector 1 1 -1
                    , vector 1 -1 -1
                    , vector -1 -1 -1
                    ]
            , vertexIndexes =
                [ [ 3, 2, 1, 0 ]
                , [ 5, 4, 0, 1 ]
                , [ 6, 5, 1, 2 ]
                , [ 7, 6, 2, 3 ]
                , [ 7, 3, 0, 4 ]
                , [ 7, 4, 5, 6 ]
                ]
            }
