module Mesh exposing (toFaces, drawable, boxes, whitelistedBoxes, MeshData, byName, options)

import Array exposing (Array)
import Dict exposing (Dict)
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


boxes : Int -> Bounds -> Drawable Vertex
boxes depth tree =
    let
        keepLeaf ( ( level, _ ), _ ) =
            level < depth

        keepInternal ( ( level, _ ), _ ) =
            level == depth - 1
    in
        boxesWithFilters keepLeaf keepInternal tree


whitelistedBoxes : Set ( Int, Int ) -> Int -> Bounds -> Drawable Vertex
whitelistedBoxes whitelist depth tree =
    let
        keepLeaf ( ( level, offset ), _ ) =
            (level < depth)
                && Set.member ( level, offset ) whitelist

        keepInternal ( ( level, offset ), _ ) =
            (level == depth - 1)
                && Set.member ( level, offset ) whitelist
    in
        boxesWithFilters keepLeaf keepInternal tree


type alias Indexed a =
    ( ( Int, Int ), a )


boxesWithFilters : (Indexed Face -> Bool) -> (Indexed BoundingBox -> Bool) -> Bounds -> Drawable Vertex
boxesWithFilters keepLeaf keepInternal tree =
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


byName : String -> Bounds
byName shapeName =
    Dict.get shapeName meshes
        |> Maybe.withDefault []
        |> Collision.create


options : List String
options =
    Dict.keys meshes


meshes : Dict String (List Face)
meshes =
    Dict.fromList
        [ ( "cube", cube )
        , ( "tetrahedron", tetrahedron )
        ]


cube : List Face
cube =
    toFaces
        { vertexPositions =
            Array.fromList
                [ Vector.vector -1 1 1
                , Vector.vector 1 1 1
                , Vector.vector 1 -1 1
                , Vector.vector -1 -1 1
                , Vector.vector -1 1 -1
                , Vector.vector 1 1 -1
                , Vector.vector 1 -1 -1
                , Vector.vector -1 -1 -1
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


tetrahedron : List Face
tetrahedron =
    toFaces
        { vertexPositions =
            Array.fromList
                [ Vector.vector (3 / sqrt 3) 0 -(3 / sqrt 24)
                , Vector.vector -(3 / sqrt 12) 1.5 -(3 / sqrt 24)
                , Vector.vector -(3 / sqrt 12) -1.5 -(3 / sqrt 24)
                , Vector.vector 0 0 (3 * sqrt 0.375)
                ]
        , vertexIndexes =
            [ [ 0, 1, 2 ]
            , [ 0, 2, 3 ]
            , [ 1, 2, 3 ]
            , [ 0, 1, 3 ]
            ]
        }


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
