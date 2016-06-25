module Transform exposing (rotationFor, toBodyFrame, fromBodyFrame, degreesFromForward)

import Vector exposing (Vector)
import Quaternion exposing (Quaternion)


type alias Body a =
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


toBodyFrame : Vector -> Body a -> Vector
toBodyFrame point body =
    Quaternion.rotateVector (Quaternion.conjugate body.orientation)
        (Vector.sub point body.position)


fromBodyFrame : Vector -> Body a -> Vector
fromBodyFrame point body =
    Quaternion.rotateVector body.orientation point
        |> Vector.add body.position


degreesFromForward : Vector -> Body a -> Float
degreesFromForward point body =
    toBodyFrame point body
        |> Vector.normalize
        |> Vector.dot (Vector.vector 0 0 -1)
        |> acos
