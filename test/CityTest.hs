module CityTest where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import City
import FRP.Helm.Color
import HelmWrapper

cityTests = [
    testCase "viewAddition is correct" $ 
        viewAddition testCity (0, 0) (WindowDimensions (100, 100)) @?= (50 - wallWidth, 50 - wallWidth),
    testCase "viewAddition is correct" $ 
        viewAddition testCity (10, 20) (WindowDimensions (100, 100)) @?= (40 - wallWidth, 30 - wallWidth),
    testCase "viewAddition is correct" $ 
        viewAddition testCity (10, 200) (WindowDimensions (100, 100)) @?= (40 - wallWidth, 0),
    testCase "viewAddition is correct" $ 
        viewAddition testCity (1000, 1000) (WindowDimensions (100, 100)) @?= (-50 + wallWidth, -50 + wallWidth),
    testCase "viewAddition is correct" $ 
        viewAddition testCity (200, 200) (WindowDimensions (100, 100)) @?= (0, 0)
    ] where 
    testCity = City "test" 1000 1000 (100, 100) black