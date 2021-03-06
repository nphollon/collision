module Collision.BoundingBox exposing (BoundingBox, collide, collideWithFace, create, encode, decode)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Vector exposing (Vector)
import Quaternion exposing (Quaternion)
import Frame exposing (Frame)
import Collision.Hull as Hull
import Collision.Face as Face exposing (Face)
import Collision.Covariance as Covariance exposing (Covariance)


type alias BoundingBox =
    { a : Float
    , b : Float
    , c : Float
    , frame : Frame
    }


encode : BoundingBox -> Value
encode box =
    Encode.object
        [ ( "a", Encode.float box.a )
        , ( "b", Encode.float box.b )
        , ( "c", Encode.float box.c )
        , ( "position", Vector.encode box.frame.position )
        , ( "orientation", Quaternion.encode box.frame.orientation )
        ]


decode : Decoder BoundingBox
decode =
    Decode.map4 BoundingBox
        (Decode.field "a" Decode.float)
        (Decode.field "b" Decode.float)
        (Decode.field "c" Decode.float)
        (Decode.map2 Frame
            (Decode.field "position" Vector.decode)
            (Decode.field "orientation" Quaternion.decode)
        )


collideWithFace : Face -> BoundingBox -> Bool
collideWithFace face box =
    let
        transformedFace =
            Face.transformInto box.frame face

        center =
            Face.center transformedFace

        isInside =
            (abs center.x < box.a)
                && (abs center.y < box.b)
                && (abs center.z < box.c)

        ppp =
            Vector.vector box.a box.b box.c

        ppm =
            Vector.vector box.a box.b -box.c

        pmp =
            Vector.vector box.a -box.b box.c

        pmm =
            Vector.vector box.a -box.b -box.c

        mpp =
            Vector.vector -box.a box.b box.c

        mpm =
            Vector.vector -box.a box.b -box.c

        mmp =
            Vector.vector -box.a -box.b box.c

        mmm =
            Vector.vector -box.a -box.b -box.c

        boxFaces =
            [ Face.face ppp pmp pmm
            , Face.face pmm ppm ppp
            , Face.face mmm mmp mpp
            , Face.face mpp mpm mmm
            , Face.face ppp ppm mpm
            , Face.face mpm mpp ppp
            , Face.face mmm pmm pmp
            , Face.face pmp mmp mmm
            , Face.face ppp mpp mmp
            , Face.face mmp pmp ppp
            , Face.face mmm mpm ppm
            , Face.face ppm pmm mmm
            ]
    in
        isInside
            || List.any (Face.collide transformedFace) boxFaces


collide : BoundingBox -> BoundingBox -> Bool
collide boxA boxB =
    let
        frame =
            Frame.mul boxB.frame (Frame.inverse boxA.frame)

        t =
            frame.position

        rotation =
            frame.orientation

        {-
           rotation =
               Quaternion.conjugate boxA.frame.orientation
                   |> Quaternion.compose boxB.frame.orientation
        -}
        r1 =
            Quaternion.rotate rotation (Vector.vector 1 0 0)

        r2 =
            Quaternion.rotate rotation (Vector.vector 0 1 0)

        r3 =
            Quaternion.rotate rotation (Vector.vector 0 0 1)

        r =
            { a11 = abs (Vector.getX r1)
            , a12 = abs (Vector.getX r2)
            , a13 = abs (Vector.getX r3)
            , a21 = abs (Vector.getY r1)
            , a22 = abs (Vector.getY r2)
            , a23 = abs (Vector.getY r3)
            , a31 = abs (Vector.getZ r1)
            , a32 = abs (Vector.getZ r2)
            , a33 = abs (Vector.getZ r3)
            , s11 = Vector.getX r1
            , s12 = Vector.getX r2
            , s13 = Vector.getX r3
            , s21 = Vector.getY r1
            , s22 = Vector.getY r2
            , s23 = Vector.getY r3
            , s31 = Vector.getZ r1
            , s32 = Vector.getZ r2
            , s33 = Vector.getZ r3
            }

        aMajor =
            abs t.x
                <= (boxA.a + boxB.a * r.a11 + boxB.b * r.a12 + boxB.c * r.a13)

        aMiddle =
            abs t.y
                <= (boxA.b + boxB.a * r.a21 + boxB.b * r.a22 + boxB.c * r.a23)

        aMinor =
            abs t.z
                <= (boxA.c + boxB.a * r.a31 + boxB.b * r.a32 + boxB.c * r.a33)

        bMajor =
            abs (t.x * r.s11 + t.y * r.s21 + t.z * r.s31)
                <= (boxB.a + boxA.a * r.a11 + boxA.b * r.a21 + boxA.c * r.a31)

        bMiddle =
            abs (t.x * r.s12 + t.y * r.s22 + t.z * r.s32)
                <= (boxB.b + boxA.a * r.a12 + boxA.b * r.a22 + boxA.c * r.a32)

        bMinor =
            abs (t.x * r.s13 + t.y * r.s23 + t.z * r.s33)
                <= (boxB.c + boxA.a * r.a13 + boxA.b * r.a23 + boxA.c * r.a33)

        aMajorBMajor =
            abs (t.z * r.s21 - t.y * r.s31)
                <= (boxA.b * r.a31 + boxA.c * r.a21 + boxB.b * r.a13 + boxB.c * r.a12)

        aMajorBMiddle =
            abs (t.z * r.s22 - t.y * r.s32)
                <= (boxA.b * r.a32 + boxA.c * r.a22 + boxB.a * r.a13 + boxB.c * r.a11)

        aMajorBMinor =
            abs (t.z * r.s23 - t.y * r.s33)
                <= (boxA.b * r.a33 + boxA.c * r.a23 + boxB.a * r.a12 + boxB.b * r.a11)

        aMiddleBMajor =
            abs (t.x * r.s31 - t.z * r.s11)
                <= (boxA.a * r.a31 + boxA.c * r.a11 + boxB.b * r.a23 + boxB.c * r.a22)

        aMiddleBMiddle =
            abs (t.x * r.s32 - t.z * r.s12)
                <= (boxA.a * r.a32 + boxA.c * r.a12 + boxB.a * r.a23 + boxB.c * r.a21)

        aMiddleBMinor =
            abs (t.x * r.s33 - t.z * r.s13)
                <= (boxA.a * r.a33 + boxA.c * r.a13 + boxB.a * r.a22 + boxB.b * r.a21)

        aMinorBMajor =
            abs (t.y * r.s11 - t.x * r.s21)
                <= (boxA.a * r.a21 + boxA.b * r.a11 + boxB.b * r.a33 + boxB.c * r.a32)

        aMinorBMiddle =
            abs (t.y * r.s12 - t.x * r.s22)
                <= (boxA.a * r.a22 + boxA.b * r.a12 + boxB.a * r.a33 + boxB.c * r.a31)

        aMinorBMinor =
            abs (t.y * r.s13 - t.x * r.s23)
                <= (boxA.a * r.a23 + boxA.b * r.a13 + boxB.a * r.a32 + boxB.b * r.a31)
    in
        aMajor
            && aMiddle
            && aMinor
            && bMajor
            && bMiddle
            && bMinor
            && aMajorBMajor
            && aMajorBMiddle
            && aMajorBMinor
            && aMiddleBMajor
            && aMiddleBMiddle
            && aMiddleBMinor
            && aMinorBMajor
            && aMinorBMiddle
            && aMinorBMinor


create : List Face -> BoundingBox
create faces =
    let
        hull =
            faces
                |> List.concatMap Face.vertexList
                |> Hull.hull

        hullPoints =
            List.concatMap Face.vertexList hull
                |> Hull.unique

        facts =
            List.map Face.getFacts hull

        center =
            facts
                |> List.foldl
                    (\fact ->
                        Vector.add (Vector.scale (fact.area) fact.center)
                    )
                    (Vector.vector 0 0 0)
                |> Vector.scale (1 / 2 / toFloat (List.length hull))

        recenter face =
            { p = Vector.sub face.p center
            , q = Vector.sub face.q center
            , r = Vector.sub face.r center
            }

        basis =
            facts
                |> List.map (\fact -> ( recenter fact.face, fact.area ))
                |> Covariance.fromMesh
                |> Covariance.eigenbasis

        orientation =
            Quaternion.conjugate (Covariance.basisToQuaternion basis)

        xProj =
            projectOnto basis.x hullPoints

        yProj =
            projectOnto basis.y hullPoints

        zProj =
            projectOnto basis.z hullPoints

        position =
            Vector.vector xProj.center yProj.center zProj.center
                |> Quaternion.rotate orientation
    in
        { a = xProj.radius
        , b = yProj.radius
        , c = zProj.radius
        , frame =
            { position = position
            , orientation = orientation
            }
        }


projectOnto : Vector -> List Vector -> { center : Float, radius : Float }
projectOnto axis cloud =
    let
        project point =
            Vector.dot point axis

        checkForExtreme x ( smallestSoFar, largestSoFar ) =
            ( min smallestSoFar x, max largestSoFar x )

        ( smallest, largest ) =
            List.foldl (project >> checkForExtreme) ( 1 / 0, -1 / 0 ) cloud
    in
        { radius = 0.5 * (largest - smallest)
        , center = 0.5 * (largest + smallest)
        }
