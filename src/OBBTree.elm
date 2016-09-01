module OBBTree exposing (OBBTree, Body, collide, create, empty, projectAndSplit, encode, decode, collisionMap)

import Set exposing (Set)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Tree exposing (Tree(..), CrossFunctions)
import BoundingBox exposing (BoundingBox)
import Face exposing (Face, FaceFacts)
import Frame exposing (Frame)
import Vector exposing (Vector)
import Quaternion exposing (Quaternion)


type alias OBBTree =
    Tree BoundingBox Face


type alias Body a =
    { a
        | frame : Frame
        , bounds : OBBTree
    }


encode : OBBTree -> Value
encode =
    Tree.encode BoundingBox.encode Face.encode


decode : Decoder OBBTree
decode =
    Tree.decode BoundingBox.decode Face.decode


collide : Body a -> Body b -> Bool
collide bodyA bodyB =
    Tree.satisfies
        (crossFunctions bodyA bodyB)
        bodyA.bounds
        bodyB.bounds


collisionMap : Body a -> Body b -> Set ( Int, Int )
collisionMap bodyA bodyB =
    Tree.collisionMap
        (crossFunctions bodyA bodyB)
        bodyA.bounds
        bodyB.bounds


crossFunctions : Body a -> Body b -> CrossFunctions BoundingBox Face BoundingBox Face
crossFunctions bodyA bodyB =
    let
        transformAFace =
            Face.transformInto bodyA.frame

        transformBFace =
            Face.transformInto bodyB.frame

        transformABox box =
            { box | frame = Frame.add bodyA.frame box.frame }

        transformBBox box =
            { box | frame = Frame.add bodyB.frame box.frame }

        boxCollide boxA boxB =
            BoundingBox.collide
                (transformABox boxA)
                (transformBBox boxB)

        faceCollide faceA faceB =
            Face.collide
                (transformAFace faceA)
                (transformBFace faceB)

        boxFaceCollide boxA faceB =
            BoundingBox.collideWithFace
                (transformBFace faceB)
                (transformABox boxA)

        faceBoxCollide faceA boxB =
            BoundingBox.collideWithFace
                (transformAFace faceA)
                (transformBBox boxB)
    in
        { nodeNode = boxCollide
        , leafLeaf = faceCollide
        , nodeLeaf = boxFaceCollide
        , leafNode = faceBoxCollide
        }


empty : OBBTree
empty =
    let
        farPoint =
            Vector.vector (1 / 0) 0 0
    in
        Leaf (Face.face farPoint farPoint farPoint)


create : List Face -> OBBTree
create faces =
    case faces of
        [] ->
            empty

        f :: [] ->
            Leaf f

        _ ->
            let
                bb =
                    BoundingBox.create faces

                ( left, right ) =
                    partitionFaces bb faces
            in
                Node bb (create left) (create right)


partitionFaces : BoundingBox -> List Face -> ( List Face, List Face )
partitionFaces box faces =
    let
        transform =
            Quaternion.rotateVector box.frame.orientation

        basis =
            [ ( box.a, Vector.xAxis )
            , ( box.b, Vector.yAxis )
            , ( box.c, Vector.zAxis )
            ]

        orderedBasis =
            List.sortWith (\a b -> compare (fst b) (fst a))
                basis

        projections =
            List.map (snd >> transform >> projectAndSplit)
                orderedBasis
    in
        List.map Face.getFacts faces
            |> tryApply projections
            |> Maybe.withDefault (simpleSplit faces)


projectAndSplit : Vector -> List FaceFacts -> Maybe ( List Face, List Face )
projectAndSplit axis factsList =
    let
        equal a b =
            (a - b) ^ 2 < 1.0e-10

        project facts =
            ( Vector.dot facts.center axis, facts )

        init =
            { firstHalf = []
            , lastHalf = []
            , splitValue = 0 / 0
            , index = 0
            , done = False
            }

        addFace projectedFacts accumulator =
            if accumulator.done then
                updateFirstHalf projectedFacts accumulator
            else
                updateBothHalves projectedFacts accumulator
                    |> checkIfDone

        updateFirstHalf ( _, facts ) acc =
            { acc | firstHalf = facts.face :: acc.firstHalf }

        updateBothHalves ( value, facts ) acc =
            if equal value acc.splitValue then
                { acc | firstHalf = facts.face :: acc.firstHalf }
            else
                { acc
                    | lastHalf = acc.firstHalf ++ acc.lastHalf
                    , firstHalf = [ facts.face ]
                    , splitValue = value
                }

        checkIfDone acc =
            if (acc.index >= limit) && not (List.isEmpty acc.lastHalf) then
                { acc | done = True }
            else
                { acc | index = acc.index + 1 }

        limit =
            List.length factsList // 2

        returnValue accumulated =
            if List.isEmpty accumulated.lastHalf then
                Nothing
            else
                Just ( accumulated.firstHalf, accumulated.lastHalf )
    in
        List.map project factsList
            |> List.sortBy fst
            |> List.foldr addFace init
            |> returnValue


tryApply : List (a -> Maybe b) -> a -> Maybe b
tryApply maybes arg =
    let
        tryAgain newFunction lastValue =
            if lastValue == Nothing then
                newFunction arg
            else
                lastValue
    in
        List.foldl tryAgain Nothing maybes


simpleSplit : List a -> ( List a, List a )
simpleSplit whole =
    let
        transfer n ( firstHalf, lastHalf ) =
            if n <= 0 then
                ( firstHalf, lastHalf )
            else
                case lastHalf of
                    [] ->
                        ( firstHalf, lastHalf )

                    x :: xs ->
                        transfer (n - 1) ( x :: firstHalf, xs )
    in
        transfer (List.length whole // 2) ( [], whole )
