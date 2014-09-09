module RectangleTest where

import           Rectangle
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit

physicsTests = [
    testCase "rectangle from bounds does not create negative width rectangle" $
        rectangleFromBounds 1 1 0 2 @?= Nothing,
    testCase "rectangle from bounds does not create zero height rectangle" $
        rectangleFromBounds 1 1 2 1 @?= Nothing,
    testCase "rectangle from bounds creates positive rectangle" $
        rectangleFromBounds (-4) (-3) (-3) (-2) @?= (Just $ Rectangle (-4, -3) (1,1)),
    testCase "rectangle intersection returns nothing for nonintersecting rectangles" $
        intersection (Rectangle (1, 1) (4, 3)) (Rectangle (7, 2) (3, 1)) @?= Nothing,
    testCase "rectangle intersection returns correct result for intersecting rectangles" $
        intersection (Rectangle (1, 1) (4, 3)) (Rectangle (4, 2) (3, 1)) @?= (Just $ Rectangle (4, 2) (1, 1)),
    testCase "collide moves rectangle" $
        collide (Rectangle (1, 5) (1, 1)) (Rectangle (1, 0) (1, 1)) (1, -1) @?= (1, -1),
    testCase "collide does not move into wall" $
        collide (Rectangle (1, 5) (1, 1)) (Rectangle (3, 0) (5, 10)) (1.5, -2) @?= (1.0, -2),
    testCase "collide does not move into wall" $
        collide (Rectangle (1, 5) (1, 1)) (Rectangle (3, 0) (5, 10)) (1.5, 2) @?= (1.0, 2),
    testCase "collide does not move into wall" $
        collide (Rectangle (5, 1) (1, 1)) (Rectangle (0, 3) (10, 5)) (-2, 1.5) @?= (-2, 1.0)
    ]
