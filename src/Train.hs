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
import Data.Traversable (sequenceA)
import Data.List (transpose)

data Train = Train { schedule :: [(City, Time)] }

trainCity :: Train -> Signal Time -> SignalGen (Signal (Maybe City))
trainCity train time = (head . snd) <~ transfer (stays, cities) step time
    where timeAtCity = 5 * second
          trainSchedule = schedule train
          cityStays = map (const timeAtCity) $ schedule train
          travelTimes = map snd $ schedule train
          stays = cycle $ concat $ transpose [cityStays, travelTimes]
          cities = cycle $ concat $ transpose [map (Just . fst) trainSchedule, replicate (length trainSchedule) Nothing] 
          step delta (currentStay:otherStays, currentCity:otherCities) = let restOfStay = currentStay - delta
                 in if restOfStay > 0 then (restOfStay:otherStays, currentCity:otherCities)
                    else step (-restOfStay) (otherStays, otherCities)

someTrains = [Train [(someCity, 10 * second), (secondCity, 10 * second)]]

renderTrain (Just city) = Just $ move (startPoint city) $ filled blue $ rect 50 50
renderTrain Nothing = Nothing

renderTrainByCity :: Signal Time -> Signal City -> Train -> SignalGen (Signal Form)
renderTrainByCity timeSignal citySignal train = do
    currentTrainCity <- trainCity train timeSignal
    let cityCheck maybeTrainCity maybeTestCity = do
            trainCity <- maybeTrainCity
            testCity <- maybeTestCity
            guard $ testCity == trainCity
            return testCity
    let cityToRender = cityCheck <$> currentTrainCity <*> (Just <$> citySignal)
    return $ group <$> maybeToList <$> (renderTrain <$> cityToRender)
    
renderTrainsByCity :: [Train] -> Signal Time -> Signal City -> SignalGen (Signal Form)
renderTrainsByCity trains time city = do
    formSignals <- mapM  (renderTrainByCity time city) trains
    return $ group <$> sequenceA formSignals
    