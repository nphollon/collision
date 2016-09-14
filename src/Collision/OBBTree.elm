module Collision.OBBTree exposing (OBBTree, Body, collide, create, empty, projectAndSplit, encode, decode, collisionMap)

import Set exposing (Set)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, (:=))
import Frame exposing (Frame)
import Vector exposing (Vector)
import Quaternion exposing (Quaternion)
import Collision.Tree as Tree exposing (Tree(..))
import Collision.BoundingBox as BoundingBox exposing (BoundingBox)
import Collision.Face as Face exposing (Face, FaceFacts)


type alias OBBTree =
    Tree BoundingBox Face


type alias Body a =
    { a
        | frame : Frame
        , bounds : OBBTree
    }


collide : Body a -> Body b -> Bool
collide bodyA bodyB =
    collideRecurse
        (crossFunctions bodyA bodyB)
        bodyA.bounds
        bodyB.bounds


collisionMap : Body a -> Body b -> Set ( Int, Int )
collisionMap bodyA bodyB =
    collisionMapRecurse
        (crossFunctions bodyA bodyB)
        bodyA.bounds
        bodyB.bounds
        ( 0, 0 )
        Set.empty


crossFunctions : Body a -> Body b -> CrossFunctions
crossFunctions bodyA bodyB =
    let
        transformAFace =
            Face.transformOutOf bodyA.frame

        transformBFace =
            Face.transformOutOf bodyB.frame

        transformABox box =
            { box | frame = Frame.compose bodyA.frame box.frame }

        transformBBox box =
            { box | frame = Frame.compose bodyB.frame box.frame }

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


type alias CrossFunctions =
    { nodeNode : BoundingBox -> BoundingBox -> Bool
    , leafLeaf : Face -> Face -> Bool
    , nodeLeaf : BoundingBox -> Face -> Bool
    , leafNode : Face -> BoundingBox -> Bool
    }


collideRecurse : CrossFunctions -> OBBTree -> OBBTree -> Bool
collideRecurse xf a b =
    let
        recurse =
            collideRecurse xf
    in
        case ( a, b ) of
            ( Leaf aVal, Leaf bVal ) ->
                xf.leafLeaf aVal bVal

            ( Leaf aVal, Node bVal bFst bSnd ) ->
                xf.leafNode aVal bVal
                    && (recurse a bFst || recurse a bSnd)

            ( Node aVal aFst aSnd, Leaf bVal ) ->
                xf.nodeLeaf aVal bVal
                    && (recurse aFst b || recurse aSnd b)

            ( Node aVal aFst aSnd, Node bVal bFst bSnd ) ->
                if xf.nodeNode aVal bVal then
                    if octantVolume aVal > octantVolume bVal then
                        (recurse aFst b) || (recurse aSnd b)
                    else
                        (recurse a bFst) || (recurse a bSnd)
                else
                    False


octantVolume : BoundingBox -> Float
octantVolume box =
    box.a * box.b * box.c


collisionMapRecurse : CrossFunctions -> OBBTree -> OBBTree -> ( Int, Int ) -> Set ( Int, Int ) -> Set ( Int, Int )
collisionMapRecurse xf a b coords hits =
    let
        recurse =
            collisionMapRecurse xf

        toTheLeft =
            Tree.toTheLeft coords

        toTheRight =
            Tree.toTheRight coords
    in
        case ( a, b ) of
            ( Leaf aVal, Leaf bVal ) ->
                if xf.leafLeaf aVal bVal then
                    Set.insert coords hits
                else
                    hits

            ( Leaf aVal, Node bVal bLeft bRight ) ->
                if xf.leafNode aVal bVal then
                    hits
                        |> recurse a bLeft coords
                        |> recurse a bRight coords
                else
                    hits

            ( Node aVal aLeft aRight, Leaf bVal ) ->
                if xf.nodeLeaf aVal bVal then
                    Set.insert coords hits
                        |> recurse aLeft b toTheLeft
                        |> recurse aRight b toTheRight
                else
                    hits

            ( Node aVal aLeft aRight, Node bVal bLeft bRight ) ->
                if xf.nodeNode aVal bVal then
                    Set.insert coords hits
                        |> recurse aLeft bLeft toTheLeft
                        |> recurse aLeft bRight toTheLeft
                        |> recurse aRight bLeft toTheRight
                        |> recurse aRight bRight toTheRight
                else
                    hits


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


encode : OBBTree -> Value
encode tree =
    case tree of
        Leaf a ->
            Encode.object
                [ ( "nodeType", Encode.string "leaf" )
                , ( "value", Face.encode a )
                ]

        Node a left right ->
            Encode.object
                [ ( "nodeType", Encode.string "internal" )
                , ( "value", BoundingBox.encode a )
                , ( "left", encode left )
                , ( "right", encode right )
                ]


decode : Decoder OBBTree
decode =
    let
        treeData nodeType =
            case nodeType of
                "leaf" ->
                    Decode.object1 Leaf ("value" := Face.decode)

                "internal" ->
                    Decode.object3 Node
                        ("value" := BoundingBox.decode)
                        ("left" := decode)
                        ("right" := decode)

                _ ->
                    Decode.fail ("unrecognized node type: " ++ nodeType)
    in
        Decode.andThen ("nodeType" := Decode.string) treeData
