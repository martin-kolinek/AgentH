module City where

import           Control.Applicative
import           FRP.Elerea.Simple
import           FRP.Helm
import           FRP.Helm.Graphics

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

someCity = City {width = 1000, height = 1000, startPoint = (100, 100)}


