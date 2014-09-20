module City where

import           Control.Applicative
import           FRP.Elerea.Simple
import           FRP.Helm
import           FRP.Helm.Graphics
import Debug.Trace
import Data.VectorSpace
import Rectangle

data City = City {cityName :: String, cityWidth :: Double, cityHeight :: Double, startPoint :: (Double, Double)} deriving (Show, Eq)

wallWidth = 20
halfWallWidth = wallWidth / 2

cityWalls City {cityWidth = width, cityHeight = height} = [
    Rectangle (-wallWidth, -wallWidth) (width + 2* wallWidth, wallWidth),
    Rectangle (-wallWidth, 0) (wallWidth, height),
    Rectangle (-wallWidth, height) (width+2*wallWidth, wallWidth),
    Rectangle (width, 0) (wallWidth, height)
    ]

cityForm :: City -> Form
cityForm city = group $ rectangleForm <$> cityWalls city 

someCity = City {cityName = "SomeCity", cityWidth = 1000, cityHeight = 1000, startPoint = (500, 500)}
secondCity = City {cityName = "SecondCity", cityWidth = 500, cityHeight = 500, startPoint = (250, 250)}

someCities = [someCity, secondCity]

cityLeft city = -wallWidth
cityTop city = -wallWidth
cityRight city = cityWidth city + wallWidth
cityBottom city = cityHeight city + wallWidth

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
