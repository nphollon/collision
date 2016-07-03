module Main exposing (..)

import ElmTest exposing (..)
import FaceTest
import TreeTest
import BoundingBoxTest
import OBBTreeTest
import CovarianceTest
import QuaternionTest


testSuite : Test
testSuite =
    suite "All tests"
        [ FaceTest.testSuite
        , TreeTest.testSuite
        , BoundingBoxTest.testSuite
        , OBBTreeTest.testSuite
        , CovarianceTest.testSuite
        , QuaternionTest.testSuite
        ]


main : Program Never
main =
    runSuite testSuite
