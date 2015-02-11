module City where

import           Control.Applicative
import           FRP.Elerea.Simple
import           FRP.Helm
import           FRP.Helm.Graphics
import Debug.Trace
import Data.VectorSpace
import Rectangle

data City = City {cityName :: String, cityWidth :: Double, cityHeight :: Double, startPoint :: (Double, Double), cityColor :: Color} deriving (Show, Eq)

wallWidth = 20
halfWallWidth = wallWidth / 2

cityWalls City {cityWidth = width, cityHeight = height} = [
    Rectangle (-wallWidth, -wallWidth) (width + 2* wallWidth, wallWidth),
    Rectangle (-wallWidth, 0) (wallWidth, height),
    Rectangle (-wallWidth, height) (width+2*wallWidth, wallWidth),
    Rectangle (width, 0) (wallWidth, height)
    ]

cityBackground City {cityWidth = width, cityHeight = height, cityColor = color} = 
    rectangleForm color $ Rectangle (0, 0) (width, height)

cityForm :: City -> Form
cityForm city = group $ cityBackground city : (rectangleForm red <$> cityWalls city)

someCity = City {cityName = "SomeCity", cityWidth = 1000, cityHeight = 1000, startPoint = (500, 500), cityColor = gray}
secondCity = City {cityName = "SecondCity", cityWidth = 500, cityHeight = 500, startPoint = (250, 250), cityColor = navy}

someCities = [someCity, secondCity]

cityLeft city = -wallWidth
cityTop city = -wallWidth
cityRight city = cityWidth city + wallWidth
cityBottom city = cityHeight city + wallWidth

viewAdditionPart cityBeginning cityEnd position windowDimension 
    | cityEnd - cityBeginning < windowDimension = 
        let cityLength = cityEnd - cityBeginning
            centeringAddition = (windowDimension - cityLength) / 2
        in -position + windowDimension / 2 + cityBeginning - centeringAddition
viewAdditionPart cityBeginning cityEnd position windowDimension = 
    let beginning = position - windowDimension / 2
        end = position + windowDimension / 2
        beginningDifference = max 0 $ cityBeginning - beginning
        endDifference = min 0 $ cityEnd - end
    in beginningDifference + endDifference
    
viewAddition city (px, py) (dx, dy) = 
    (viewAdditionPart (cityLeft city) (cityRight city) px dx, 
        viewAdditionPart (cityTop city) (cityBottom city) py dy)
