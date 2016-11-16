module Assertion exposing (..)

import Expect exposing (..)
import Vector as Vec exposing (Vector)
import Quaternion exposing (Quaternion)


equalFloat : Float -> Float -> Bool
equalFloat a b =
    (a - b) ^ 2 < 1.0e-10


equalVector : Vector -> Vector -> Bool
equalVector a b =
    equalFloat 0 (Vec.distanceSquared a b)


equalPair : ( Float, Float ) -> ( Float, Float ) -> Bool
equalPair a b =
    equalFloat (Tuple.first a) (Tuple.first b) && equalFloat (Tuple.second a) (Tuple.second b)


equalQuaternion : Quaternion -> Quaternion -> Bool
equalQuaternion a b =
    (equalVector a.vector b.vector) && (equalFloat a.scalar b.scalar)


assertEqualFloat : Float -> Float -> Expectation
assertEqualFloat a b =
    if equalFloat a b then
        pass
    else
        equal a b


assertEqualVector : Vector -> Vector -> Expectation
assertEqualVector a b =
    if equalVector a b then
        pass
    else
        equal a b


assertEqualPair : ( Float, Float ) -> ( Float, Float ) -> Expectation
assertEqualPair a b =
    if equalPair a b then
        pass
    else
        equal a b


assertEqualQuaternion : Quaternion -> Quaternion -> Expectation
assertEqualQuaternion a b =
    if equalQuaternion a b then
        pass
    else
        equal a b


assertListContents : List a -> List a -> Expectation
assertListContents expected actual =
    let
        extra =
            List.filter (\i -> not (List.member i expected)) actual

        missing =
            List.filter (\i -> not (List.member i actual)) expected
    in
        if List.isEmpty extra && List.isEmpty missing then
            pass
        else
            fail <|
                "Result list was missing "
                    ++ toString missing
                    ++ " and should not have contained "
                    ++ toString extra
