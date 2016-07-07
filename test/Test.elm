module Main exposing (..)

import ElmTest exposing (..)
import QuaternionTest
import FaceTest
import TreeTest
import HullTest
import CovarianceTest
import BoundingBoxTest
import OBBTreeTest


testSuite : Test
testSuite =
    suite "All tests"
        [ FaceTest.testSuite
        , QuaternionTest.testSuite
        , TreeTest.testSuite
        , HullTest.testSuite
        , CovarianceTest.testSuite
        , BoundingBoxTest.testSuite
        , OBBTreeTest.testSuite
        ]


main : Program Never
main =
    runSuite testSuite
