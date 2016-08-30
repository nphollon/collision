module Model exposing (toFaces, drawable, MeshData)

import Array exposing (Array)
import Maybe.Extra as MaybeX
import Math.Vector3 as Vec3 exposing (Vec3)
import WebGL exposing (Drawable(..))


-- Collision Library

import Vector exposing (Vector)
import Face exposing (Face)


-- Project local

import Types exposing (..)


type alias MeshData =
    { vertexPositions : Array Vector
    , vertexIndexes : List (List Int)
    }


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
