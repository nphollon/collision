module Main exposing (..)

import ElmTest exposing (..)
import FaceTest
import TreeTest
import BoundingBoxTest
import CovarianceTest
import QuaternionTest


testSuite : Test
testSuite =
    suite "All tests"
        [ FaceTest.testSuite
        , TreeTest.testSuite
        , BoundingBoxTest.testSuite
        , CovarianceTest.testSuite
        , QuaternionTest.testSuite
        ]


main : Program Never
main =
    runSuite testSuite
