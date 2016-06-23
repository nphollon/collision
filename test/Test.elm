module Main exposing (..)

import ElmTest exposing (..)
import TreeTest
import BoundingBoxTest
import CovarianceTest
import TransformTest


testSuite : Test
testSuite =
    suite "All tests"
        [ TreeTest.testSuite
        , BoundingBoxTest.testSuite
        , CovarianceTest.testSuite
        , TransformTest.testSuite
        ]


main : Program Never
main =
    runSuite testSuite
