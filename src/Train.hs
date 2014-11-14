{-# LANGUAGE TupleSections #-}
module Train where

import FRP.Helm.Time
import FRP.Helm.Graphics
import FRP.Helm.Color
import FRP.Helm
import FRP.Elerea.Simple
import City
import Control.Monad
import Control.Applicative
import Data.Fixed
import Data.Maybe
import Debug.Trace
import Data.Traversable (sequenceA)
import Data.List (transpose, find)
import Rectangle

data Train = Train { schedule :: [(City, Time)] } deriving (Eq, Show)

trainCity :: Train -> Signal Time -> SignalGen (Signal (Maybe City))
trainCity train deltaSignal = (head . snd) <~ transfer (stays, cities) step deltaSignal
    where timeAtCity = 5 * second
          trainSchedule = schedule train
          cityStays = map (const timeAtCity) $ schedule train
          travelTimes = map snd $ schedule train
          stays = cycle $ concat $ transpose [cityStays, travelTimes]
          cities = cycle $ concat $ transpose [map (Just . fst) trainSchedule, replicate (length trainSchedule) Nothing] 
          step delta (currentStay:otherStays, currentCity:otherCities) = let restOfStay = currentStay - delta
                 in if restOfStay > 0 then (restOfStay:otherStays, currentCity:otherCities)
                    else step (-restOfStay) (otherStays, otherCities)

trainRectangle city = moveRectangle (Rectangle (-25, -25) (50, 50)) (startPoint city)

someTrains = [Train [(someCity, 3 * second), (secondCity, 3 * second)]]

renderTrain (Just city) = Just $ rectangleForm blue $ trainRectangle city
renderTrain Nothing = Nothing

trainLeaving :: Train -> Signal Time -> SignalGen (Signal (Maybe City))
trainLeaving train time = do
    let slide2 current (_, last) = (last, current)
        detectLeaving (last, Nothing) = last
        detectLeaving _ = Nothing
    slidingSignal <- foldp slide2 (Nothing, Nothing) $ trainCity train time
    return $ detectLeaving <$> slidingSignal

cityLeavingTrain :: Signal (Maybe City) -> Signal Time -> SignalGen (Signal (Maybe Train))
cityLeavingTrain citySignal deltaSignal = do 
    let cityFilter train city1 city2 
                | city1 == city2 = Just train 
                | otherwise = Nothing
        trainLeavingCity train = do
            trainLeavingSignal <- trainLeaving train deltaSignal
            return $ cityFilter train <$> citySignal <*> trainLeavingSignal 
    leavingTrainMaybes <- liftM sequenceA $ mapM trainLeavingCity someTrains
    return $ join <$> find isJust <$> leavingTrainMaybes
     
renderTrainByCity :: Signal Time -> Signal (Maybe City) -> Train -> SignalGen (Signal Form)
renderTrainByCity deltaSignal citySignal train = do
    currentTrainCity <- trainCity train deltaSignal
    let cityCheck maybeTrainCity maybeTestCity = do
            trainCity <- maybeTrainCity
            testCity <- maybeTestCity
            guard $ testCity == trainCity
            return testCity
    let cityToRender = cityCheck <$> currentTrainCity <*> citySignal
    return $ group <$> maybeToList <$> (renderTrain <$> cityToRender)
    
renderTrainsByCity :: [Train] -> Signal Time -> Signal (Maybe City) -> SignalGen (Signal Form)
renderTrainsByCity trains time city = do
    formSignals <- mapM (renderTrainByCity time city) trains
    return $ group <$> sequenceA formSignals
    