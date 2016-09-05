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
        , ( "icosahedron", icosahedron )
        , ( "ring", ring )
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


icosahedron : List Face
icosahedron =
    toFaces
        { vertexPositions =
            [ Vector.vector 0 0 0
            , Vector.vector 0 -1 0
            , Vector.vector 0.7236 -0.447215 0.52572
            , Vector.vector -0.276385 -0.447215 0.85064
            , Vector.vector -0.894425 -0.447215 0
            , Vector.vector -0.276385 -0.447215 -0.85064
            , Vector.vector 0.7236 -0.447215 -0.52572
            , Vector.vector 0.276385 0.447215 0.85064
            , Vector.vector -0.7236 0.447215 0.52572
            , Vector.vector -0.7236 0.447215 -0.52572
            , Vector.vector 0.276385 0.447215 -0.85064
            , Vector.vector 0.894425 0.447215 0
            , Vector.vector 0 1 0
            ]
                |> List.map (Vector.scale 2)
                |> Array.fromList
        , vertexIndexes =
            [ [ 1, 2, 3 ]
            , [ 2, 1, 6 ]
            , [ 1, 3, 4 ]
            , [ 1, 4, 5 ]
            , [ 1, 5, 6 ]
            , [ 2, 6, 11 ]
            , [ 3, 2, 7 ]
            , [ 4, 3, 8 ]
            , [ 5, 4, 9 ]
            , [ 6, 5, 10 ]
            , [ 2, 11, 7 ]
            , [ 3, 7, 8 ]
            , [ 4, 8, 9 ]
            , [ 5, 9, 10 ]
            , [ 6, 10, 11 ]
            , [ 7, 11, 12 ]
            , [ 8, 7, 12 ]
            , [ 9, 8, 12 ]
            , [ 10, 9, 12 ]
            , [ 11, 10, 12 ]
            ]
        }


ring : List Face
ring =
    toFaces
        { vertexPositions =
            [ Vector.vector 0 0 0
            , Vector.vector 1.25 0 0
            , Vector.vector 1 0.25 0
            , Vector.vector 0.75 0 0
            , Vector.vector 1 -0.25 0
            , Vector.vector 0 0 -1.25
            , Vector.vector 0 0.25 -1
            , Vector.vector 0 0 -0.75
            , Vector.vector 0 -0.25 -1
            , Vector.vector -1.25 0 0
            , Vector.vector -1 0.25 0
            , Vector.vector -0.75 0 0
            , Vector.vector -1 -0.25 0
            , Vector.vector 0 0 1.25
            , Vector.vector 0 0.25 1
            , Vector.vector 0 0 0.75
            , Vector.vector 0 -0.25 1
            ]
                |> List.map (Vector.scale 2)
                |> Array.fromList
        , vertexIndexes =
            [ [ 1, 5, 6, 2 ]
            , [ 2, 6, 7, 3 ]
            , [ 3, 7, 8, 4 ]
            , [ 1, 4, 8, 5 ]
            , [ 5, 9, 10, 6 ]
            , [ 6, 10, 11, 7 ]
            , [ 7, 11, 12, 8 ]
            , [ 8, 12, 9, 5 ]
            , [ 9, 13, 14, 10 ]
            , [ 10, 14, 15, 11 ]
            , [ 11, 15, 16, 12 ]
            , [ 12, 16, 13, 9 ]
            , [ 13, 1, 2, 14 ]
            , [ 14, 2, 3, 15 ]
            , [ 15, 3, 4, 16 ]
            , [ 16, 4, 1, 13 ]
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
