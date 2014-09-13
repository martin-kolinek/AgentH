module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit
import RectangleTest
import CityTest

testCyclicOneShiftRightHU =
    "Cyclic one shift right" ~: [4,1,2,3]  @=? [4,1,2,3]

main = defaultMain tests

tests = [
    testGroup "physics" physicsTests,
    testGroup "city" cityTests
    ]

