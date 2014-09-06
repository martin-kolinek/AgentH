module City where

import           Control.Applicative
import           FRP.Elerea.Simple
import           FRP.Helm
import           FRP.Helm.Graphics
import Debug.Trace
import Data.VectorSpace

data City = City {width :: Double, height :: Double, startPoint :: (Double, Double)}

wallWidth = 20
halfWallWidth = wallWidth / 2

cityForm :: City -> Form
cityForm City {width = width, height = height} = group [
    move (width/2, -halfWallWidth) $ filled white $ rect width wallWidth,
    move (width + halfWallWidth, height/2) $ filled white $ rect wallWidth height,
    move (width/2, height + halfWallWidth) $ filled white $ rect width wallWidth,
    move (-halfWallWidth, height/2) $ filled white $ rect wallWidth height,
    move (-halfWallWidth, -halfWallWidth) $ filled white $ square wallWidth,
    move (width + halfWallWidth, -halfWallWidth) $ filled white $ square wallWidth,
    move (width + halfWallWidth, height + halfWallWidth) $ filled white $ square wallWidth,
    move (-halfWallWidth, height + halfWallWidth) $ filled white $ square wallWidth
    ]

someCity = City {width = 2000, height = 2000, startPoint = (1000, 1000)}

cityLeft city = -wallWidth
cityTop city = -wallWidth
cityRight city = width city + wallWidth
cityBottom city = width city + wallWidth

viewAddition city (px, py) (dx, dy) = 
    let left = px - dx/2
        right = px + dx/2
        top = py - dy/2
        bottom = py + dy/2
        leftD = max 0 $ cityLeft city - left
        topD = max 0 $ cityTop city - top
        bottomD = min 0 $ cityBottom city - bottom
        rightD = min 0 $ cityRight city - right
    in (leftD + rightD, topD + bottomD)
