module OBBTree exposing (OBBTree, Body, collide, create, projectAndSplit, encode, decode)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Tree exposing (Tree(..))
import BoundingBox exposing (BoundingBox)
import Transform
import Face exposing (Face, FaceFacts)
import Quaternion exposing (Quaternion)
import Vector exposing (Vector)


type alias OBBTree =
    Tree BoundingBox BoundingBox


type alias Body a =
    { a
        | position : Vector
        , orientation : Quaternion
        , bounds : Maybe OBBTree
    }


encode : OBBTree -> Value
encode =
    Tree.encode BoundingBox.encode BoundingBox.encode


decode : Decoder OBBTree
decode =
    Tree.decode BoundingBox.decode BoundingBox.decode


collide : Body a -> Body b -> Bool
collide bodyA bodyB =
    let
        condition boxA boxB =
            BoundingBox.collide (Transform.add bodyA boxA)
                (Transform.add bodyB boxB)
    in
        Maybe.map2 (Tree.satisfies condition condition condition)
            bodyA.bounds
            bodyB.bounds
            |> Maybe.withDefault False


create : Int -> List Face -> OBBTree
create iter faces =
    let
        bb =
            BoundingBox.create faces
    in
        if iter <= 0 then
            Leaf bb
        else
            case partitionFaces bb faces of
                Nothing ->
                    Leaf bb

                Just ( left, right ) ->
                    (Node bb) (create (iter - 1) left)
                        (create (iter - 1) right)


partitionFaces : BoundingBox -> List Face -> Maybe ( List Face, List Face )
partitionFaces box faces =
    let
        transform =
            Quaternion.rotateVector box.orientation

        basis =
            [ ( box.a, Vector.vector 1 0 0 )
            , ( box.b, Vector.vector 0 1 0 )
            , ( box.c, Vector.vector 0 0 1 )
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
