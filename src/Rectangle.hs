{-# LANGUAGE TupleSections #-}
module Rectangle where

import Data.VectorSpace
import Control.Monad
import Data.Maybe
import Data.Functor
import           FRP.Helm
import           FRP.Helm.Graphics

data Rectangle = Rectangle {position:: (Double, Double), size:: (Double, Double)} deriving (Show, Eq)

allPositive (x, y) = x > 0 && y > 0

rectangleFromBounds left top right bottom = do
    rect <- Just Rectangle {position = (left, top), size = (right - left, bottom - top)}
    guard $ allPositive $ size rect
    return rect 

moveRectangle :: Rectangle -> (Double, Double) -> Rectangle
moveRectangle rectangle dif = rectangle{position = position rectangle ^+^ dif}

left rectangle = fst $ position rectangle
top rectangle = snd $ position rectangle
right rectangle = fst $ position rectangle ^+^ size rectangle
bottom rectangle = snd $ position rectangle ^+^ size rectangle
width rectangle = fst $ size rectangle
height rectangle = snd $ size rectangle

intersection :: Rectangle -> Rectangle -> Maybe Rectangle
intersection rect1 rect2 = rectangleFromBounds maxLeft maxTop minRight minBottom
    where left1 = left rect1
          right1 = right rect1
          top1 = top rect1
          bottom1 = bottom rect1
          left2 = left rect2
          right2 = right rect2
          top2 = top rect2
          bottom2 = bottom rect2
          maxLeft = max left1 left2
          minRight = min right1 right2
          maxTop = max top1 top2
          minBottom = min bottom1 bottom2

collide :: Rectangle -> Rectangle -> (Double, Double) -> (Double, Double)
collide collider wall velocity = 
    let moved = moveRectangle collider velocity
        collision = intersection moved wall
        cWidth = fromMaybe 0 $ width <$> collision
        cHeight = fromMaybe 0 $ height <$> collision
        adjustment = if cWidth < cHeight 
            then (signum (fst velocity) * cWidth, 0) 
            else (0, signum (snd velocity) * cHeight)
    in velocity ^-^ adjustment
    
rectangleForm color (Rectangle position size@(w, h)) = move (position ^+^ (size ^/ 2)) $ filled color $ rect w h
