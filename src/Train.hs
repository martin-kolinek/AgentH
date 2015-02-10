{-# LANGUAGE TupleSections #-}
module Train where

import Prelude hiding (sequence)
import FRP.Helm.Time
import FRP.Helm.Graphics
import FRP.Helm.Color
import FRP.Helm
import FRP.Elerea.Simple
import City
import Control.Monad (join, liftM, guard)
import Control.Applicative
import Data.Fixed
import Data.Maybe
import Debug.Trace
import Data.Traversable (sequenceA, sequence)
import Data.List (transpose, find)
import Rectangle
import qualified Data.Map.Strict as M
import qualified Control.Arrow as A 

data Train = Train { schedule :: TrainSchedule, trainId :: TrainId } deriving (Eq, Show)

newtype TrainId = TrainId Int deriving (Eq, Show, Ord)

data TrainCollection = TrainCollection { collectionTrains :: M.Map TrainId TrainDescriptor }

data TrainPosition = EnteringCity City Double | TrainInCity City | LeavingCity City Double | BetweenCities

data TrainDescriptor = TrainDescriptor {
    currentPositionSignal :: Signal TrainPosition,
    citiesEnteringSignal :: Signal (Maybe City),
    citiesLeavingSignal :: Signal (Maybe City)
}

newtype TrainSchedule = TrainSchedule [(City, Time)] deriving (Eq, Show)

createTrainCollection :: [Train] -> SignalGen TrainCollection
createTrainCollection trains = do
    let processSchedule Train { schedule = schedule, trainId = trainId } = do
            descriptor <- createTrainDescriptor schedule
            return (trainId, descriptor)
    listOfDescriptors <- mapM processSchedule trains
    return $ TrainCollection $ M.fromList listOfDescriptors

createTrainDescriptor :: TrainSchedule -> SignalGen TrainDescriptor
createTrainDescriptor schedule = do
    deltaSignal <- delta
    currentCitySignal <- trainCity deltaSignal schedule
    let slidingStep current (_, last) = (last, current)
    slidingSignal <- transfer (BetweenCities, BetweenCities) slidingStep currentCitySignal
    let enteringFilter (TrainInCity old, TrainInCity new) | old /= new = Just new
        enteringFilter (TrainInCity _, TrainInCity _) = Nothing
        enteringFilter (_, TrainInCity new) = Just new
        enteringFilter _ = Nothing
        leavingFilter (TrainInCity old, TrainInCity new) | old /= new = Just old
        leavingFilter (TrainInCity _, TrainInCity _) = Nothing
        leavingFilter (TrainInCity old, _) = Just old
        leavingFilter _ = Nothing
    let enteringSignal = enteringFilter <$> slidingSignal
    let leavingSignal = leavingFilter <$> slidingSignal 
    return (TrainDescriptor currentCitySignal enteringSignal leavingSignal)

trainCity :: Signal Time -> TrainSchedule -> SignalGen (Signal TrainPosition)
trainCity deltaSignal (TrainSchedule  trainSchedule) = (head . snd) <~ transfer (stays, cities) step deltaSignal
    where timeAtCity = 5 * second
          cityStays = map (const timeAtCity) trainSchedule
          travelTimes = map snd trainSchedule
          stays = cycle $ concat $ transpose [cityStays, travelTimes]
          cities = cycle $ concat $ transpose [map (TrainInCity . fst) trainSchedule, replicate (length trainSchedule) BetweenCities] 
          step delta (currentStay:otherStays, currentCity:otherCities) = let restOfStay = currentStay - delta
                 in if restOfStay > 0 then (restOfStay:otherStays, currentCity:otherCities)
                    else step (-restOfStay) (otherStays, otherCities)
 
trainRectangle city = moveRectangle (Rectangle (-25, -25) (50, 50)) (startPoint city)
          
someTrains = [Train (TrainSchedule [(someCity, 3 * second), (secondCity, 3 * second)]) (TrainId 1)]

globalTrainCollection = createTrainCollection someTrains

cityLeavingTrain :: TrainCollection -> Signal (Maybe City) -> Signal (Maybe TrainId) 
cityLeavingTrain (TrainCollection descriptors) citySignal = 
    let strength (x, y) = (x,) <$> y
        leavingsSignal = sequenceA $ strength . A.second citiesLeavingSignal <$> M.toList descriptors
        matchCity (Just desiredCity) (_, Just otherCity) = desiredCity == otherCity
        matchCity _ _ = False
        findTrainId desiredCity cities = fst <$> find (matchCity desiredCity) cities
    in findTrainId <$> citySignal <*> leavingsSignal

trainComing :: TrainCollection -> Signal (Maybe TrainId) -> Signal (Maybe City)
trainComing (TrainCollection descriptors) trainSignal =
    let findTrainSignal (Just trainId) = citiesEnteringSignal $ descriptors M.! trainId
        findTrainSignal _ = pure Nothing
    in trainSignal >>= findTrainSignal

renderTrain (Just city) = Just $ rectangleForm blue $ trainRectangle city
renderTrain Nothing = Nothing

renderTrainByCity :: City -> TrainDescriptor -> Signal Form
renderTrainByCity city trainDescriptor =
    let currentTrainCity = currentPositionSignal trainDescriptor
        cityCheck testCity (TrainInCity trainCity) | testCity == trainCity = Just testCity
        cityCheck _ _ = Nothing
        cityToRender = cityCheck city <$> currentTrainCity
    in group <$> maybeToList <$> (renderTrain <$> cityToRender)
    
renderTrainsByCity :: TrainCollection -> City -> Signal Form
renderTrainsByCity trains city =
    let trainFormSignals =  M.elems $ M.map (renderTrainByCity city) (collectionTrains trains)
    in group <$> sequenceA trainFormSignals
