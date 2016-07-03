module Face exposing (Face, face, vertexList, vertexTuple, cross, collide, encode, decode)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, (:=))
import Vector exposing (Vector)


type alias Face =
    { p : Vector
    , q : Vector
    , r : Vector
    }


face : Vector -> Vector -> Vector -> Face
face p q r =
    { p = p
    , q = q
    , r = r
    }


encode : Face -> Value
encode face =
    Encode.object
        [ ( "p", Vector.encode face.p )
        , ( "q", Vector.encode face.q )
        , ( "r", Vector.encode face.r )
        ]


decode : Decoder Face
decode =
    Decode.object3 face
        ("p" := Vector.decode)
        ("q" := Vector.decode)
        ("r" := Vector.decode)


vertexList : Face -> List Vector
vertexList face =
    [ face.p, face.q, face.r ]


vertexTuple : Face -> ( Vector, Vector, Vector )
vertexTuple face =
    ( face.p, face.q, face.r )


cross : Face -> Vector
cross { p, q, r } =
    Vector.cross (Vector.sub q p) (Vector.sub r p)


collide : Face -> Face -> Bool
collide a b =
    let
        nB =
            Vector.cross (Vector.sub b.p b.r) (Vector.sub b.q b.r)

        distA =
            { p = Vector.dot nB (Vector.sub a.p b.r)
            , q = Vector.dot nB (Vector.sub a.q b.r)
            , r = Vector.dot nB (Vector.sub a.r b.r)
            }
    in
        if sameSign distA then
            False
        else
            canonizeFaces distA ( a, b )
                |> uncurry canonicalCollide


canonicalCollide : Face -> Face -> Bool
canonicalCollide a b =
    let
        nA =
            Vector.cross (Vector.sub a.q a.p) (Vector.sub a.r a.p)

        distB =
            { p = Vector.dot nA (Vector.sub b.p a.r)
            , q = Vector.dot nA (Vector.sub b.q a.r)
            , r = Vector.dot nA (Vector.sub b.r a.r)
            }
    in
        if sameSign distB then
            False
        else
            canonizeFaces distB ( b, a )
                |> uncurry doubleCanonicalCollide


doubleCanonicalCollide : Face -> Face -> Bool
doubleCanonicalCollide a b =
    if 0 < translatedTripleProduct a.q b.q b.p a.p then
        False
    else if 0 < translatedTripleProduct a.p b.r b.p a.r then
        False
    else
        True


translatedTripleProduct : Vector -> Vector -> Vector -> Vector -> Float
translatedTripleProduct offset a b c =
    Vector.dot (Vector.sub a offset)
        (Vector.cross (Vector.sub b offset) (Vector.sub c offset))


sameSign : { p : Float, q : Float, r : Float } -> Bool
sameSign { p, q, r } =
    p * q > 0 && p * r > 0


canonizeFaces : { p : Float, q : Float, r : Float } -> ( Face, Face ) -> ( Face, Face )
canonizeFaces dist ( a, b ) =
    case ( compare dist.p 0, compare dist.q 0, compare dist.r 0 ) of
        ( GT, GT, _ ) ->
            ( rollToR a, swapQR b )

        ( GT, _, GT ) ->
            ( rollToQ a, swapQR b )

        ( GT, _, _ ) ->
            ( a, b )

        ( LT, LT, _ ) ->
            ( rollToR a, b )

        ( LT, _, LT ) ->
            ( rollToQ a, b )

        ( LT, _, _ ) ->
            ( a, swapQR b )

        ( EQ, GT, GT ) ->
            ( a, swapQR b )

        ( EQ, GT, _ ) ->
            ( rollToQ a, b )

        ( EQ, LT, LT ) ->
            ( a, b )

        ( EQ, LT, _ ) ->
            ( rollToQ a, swapQR b )

        ( EQ, EQ, GT ) ->
            ( rollToR a, b )

        ( EQ, EQ, LT ) ->
            ( rollToR a, swapQR b )

        ( EQ, EQ, EQ ) ->
            Debug.crash "this is bad"


rollToR : Face -> Face
rollToR { p, q, r } =
    { p = r, q = p, r = q }


rollToQ : Face -> Face
rollToQ { p, q, r } =
    { p = q, q = r, r = p }


swapQR : Face -> Face
swapQR { p, q, r } =
    { p = p, q = r, r = q }
