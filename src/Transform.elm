module Transform exposing (Orientable, rotationFor, toBodyFrame, fromBodyFrame, degreesFromForward, add, faceToBodyFrame, faceFromBodyFrame)

import Face exposing (Face)
import Vector exposing (Vector)
import Quaternion exposing (Quaternion)


type alias Orientable a =
    { a
        | position : Vector
        , orientation : Quaternion
    }


rotationFor : Vector -> Vector -> Quaternion
rotationFor u v =
    let
        cross =
            Vector.cross u v

        crossMag =
            Vector.length cross

        angle =
            atan2 crossMag (Vector.dot u v)
    in
        if angle == 0 then
            Quaternion.quaternion 1 0 0 0
        else if crossMag == 0 then
            Vector.vector 1.0e-10 0 0
                |> Vector.add v
                |> rotationFor u
        else
            Vector.scale (angle / crossMag) cross
                |> Quaternion.fromVector


toBodyFrame : Orientable a -> Vector -> Vector
toBodyFrame body point =
    Quaternion.rotateVector (Quaternion.conjugate body.orientation)
        (Vector.sub point body.position)


fromBodyFrame : Orientable a -> Vector -> Vector
fromBodyFrame body point =
    Quaternion.rotateVector body.orientation point
        |> Vector.add body.position


faceFromBodyFrame : Orientable a -> Face -> Face
faceFromBodyFrame body face =
    { p = fromBodyFrame body face.p
    , q = fromBodyFrame body face.q
    , r = fromBodyFrame body face.r
    }


faceToBodyFrame : Orientable a -> Face -> Face
faceToBodyFrame body face =
    { p = toBodyFrame body face.p
    , q = toBodyFrame body face.q
    , r = toBodyFrame body face.r
    }


degreesFromForward : Orientable a -> Vector -> Float
degreesFromForward body point =
    toBodyFrame body point
        |> Vector.normalize
        |> Vector.dot (Vector.vector 0 0 -1)
        |> acos


add : Orientable a -> Orientable b -> Orientable b
add { position, orientation } addend =
    { addend
        | position = Vector.add position addend.position
        , orientation = Quaternion.compose addend.orientation orientation
    }
