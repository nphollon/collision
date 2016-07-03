module Quaternion exposing (Quaternion, toVector, fromVector, fromBasis, fromAxisAngle, compose, rotateVector, quaternion, conjugate, encode, decode)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Vector exposing (Vector)


type alias Quaternion =
    { vector : Vector
    , scalar : Float
    }


encode : Quaternion -> Value
encode q =
    Encode.list
        [ Encode.float q.scalar
        , Encode.float (Vector.getX q.vector)
        , Encode.float (Vector.getY q.vector)
        , Encode.float (Vector.getZ q.vector)
        ]


decode : Decoder Quaternion
decode =
    Decode.tuple4 quaternion
        Decode.float
        Decode.float
        Decode.float
        Decode.float


quaternion : Float -> Float -> Float -> Float -> Quaternion
quaternion w x y z =
    { vector = Vector.vector x y z
    , scalar = w
    }


compose : Quaternion -> Quaternion -> Quaternion
compose p q =
    { vector =
        (Vector.scale q.scalar p.vector)
            `Vector.add` (Vector.scale p.scalar q.vector)
            `Vector.add` (q.vector `Vector.cross` p.vector)
    , scalar =
        (q.scalar * p.scalar) - (q.vector `Vector.dot` p.vector)
    }


rotateVector : Quaternion -> Vector -> Vector
rotateVector q v =
    let
        vectorQuat =
            { vector = v
            , scalar = 0
            }
    in
        compose vectorQuat q
            |> compose (conjugate q)
            |> .vector


conjugate : Quaternion -> Quaternion
conjugate q =
    { vector = Vector.negate q.vector
    , scalar = q.scalar
    }


fromVector : Vector -> Quaternion
fromVector v =
    let
        angle =
            Vector.length v
    in
        if angle == 0 then
            { vector = v
            , scalar = 1
            }
        else
            fromAxisAngle v angle


fromAxisAngle : Vector -> Float -> Quaternion
fromAxisAngle axis angle =
    { vector = Vector.scale (sin (0.5 * angle)) (Vector.normalize axis)
    , scalar = cos (0.5 * angle)
    }


toVector : Quaternion -> Vector
toVector q =
    let
        halfSin =
            Vector.length q.vector
    in
        if halfSin == 0 then
            q.vector
        else
            Vector.scale (2 * acos q.scalar / halfSin) q.vector


type alias Basis =
    { x : Vector
    , y : Vector
    , z : Vector
    }


fromBasis : Basis -> Quaternion
fromBasis basis =
    let
        diagX =
            Vector.getX basis.x

        diagY =
            Vector.getY basis.y

        diagZ =
            Vector.getZ basis.z

        pairXY =
            ( Vector.getX basis.y, Vector.getY basis.x )

        pairYZ =
            ( Vector.getY basis.z, Vector.getZ basis.y )

        pairZX =
            ( Vector.getZ basis.x, Vector.getX basis.z )

        trace =
            diagX + diagY + diagZ

        maxDiag =
            max (max diagX diagY) diagZ

        normalAdd denominator pair =
            (uncurry (+) pair) / denominator

        normalSub denominator pair =
            (uncurry (-) pair) / denominator

        doubleSqrt =
            max 0 >> sqrt >> (*) 2
    in
        if trace > 0 then
            let
                s =
                    doubleSqrt (trace + 1)
            in
                quaternion (s / 4)
                    (normalSub s pairYZ)
                    (normalSub s pairZX)
                    (normalSub s pairXY)
        else if maxDiag == diagX then
            let
                s =
                    doubleSqrt (1 + diagX - diagY - diagZ)
            in
                quaternion (normalSub s pairYZ)
                    (s / 4)
                    (normalAdd s pairXY)
                    (normalAdd s pairZX)
        else if maxDiag == diagY then
            let
                s =
                    doubleSqrt (1 - diagX + diagY - diagZ)
            in
                quaternion (normalSub s pairZX)
                    (normalAdd s pairXY)
                    (s / 4)
                    (normalAdd s pairYZ)
        else
            let
                s =
                    doubleSqrt (1 - diagX - diagY + diagZ)
            in
                quaternion (normalSub s pairXY)
                    (normalAdd s pairZX)
                    (normalAdd s pairYZ)
                    (s / 4)
