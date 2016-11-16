port module Main exposing (..)

import Test exposing (describe)
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import FaceTest
import TreeTest
import HullTest
import CovarianceTest
import BoundingBoxTest
import OBBTreeTest


main =
    run emit <|
        describe "All tests"
            [ FaceTest.testSuite
            , TreeTest.testSuite
            , HullTest.testSuite
            , CovarianceTest.testSuite
            , BoundingBoxTest.testSuite
            , OBBTreeTest.testSuite
            ]


port emit : ( String, Value ) -> Cmd msg
