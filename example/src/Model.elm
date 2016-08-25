module Model exposing (drawable, Vertex, MeshData)

import Array exposing (Array)
import Maybe.Extra as MaybeX
import Math.Vector3 as Vec3 exposing (Vec3)
import Vector exposing (Vector)
import Face exposing (Face)
import WebGL exposing (Drawable(..))


type alias Vertex =
    { position : Vec3
    , normal : Vec3
    }


type alias MeshData =
    { vertexPositions : Array Vector
    , vertexIndexes : List (List Int)
    }


drawable : MeshData -> Drawable Vertex
drawable data =
    toFaces data
        |> List.map toVertexTriangle
        |> Triangle


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


toVertexTriangle : Face -> ( Vertex, Vertex, Vertex )
toVertexTriangle face =
    let
        normal =
            Vector.normalize (Face.cross face)
    in
        (,,) (toVertex face.p normal)
            (toVertex face.q normal)
            (toVertex face.r normal)


toVertex : Vector -> Vector -> Vertex
toVertex position normal =
    { position = Vec3.fromRecord position
    , normal = Vec3.fromRecord normal
    }
