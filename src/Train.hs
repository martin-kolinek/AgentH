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
import Data.Traversable (sequenceA, sequence)
import Data.List (transpose, find)
import Data.VectorSpace
import Rectangle
import qualified Data.Map.Strict as M
import qualified Control.Arrow as A 

data Train = Train { schedule :: TrainSchedule, trainId :: TrainId } deriving (Eq, Show)

newtype TrainId = TrainId Int deriving (Eq, Show, Ord)

data TrainCollection = TrainCollection { collectionTrains :: M.Map TrainId TrainDescriptor }

type TrainPositionInCity = Double

data TrainPosition = TrainInCity City | BetweenCities | ArivingCity City TrainPositionInCity | LeavingCity City TrainPositionInCity deriving (Show)

positionCity (TrainInCity city) = Just city
positionCity (ArivingCity city _) = Just city
positionCity (LeavingCity city _) = Just city
positionCity _ = Nothing

data TrainDescriptor = TrainDescriptor {
    currentPositionSignal :: Signal TrainPosition,
    citiesEnteringSignal :: Signal (Maybe City),
    citiesLeavingSignal :: Signal (Maybe City)
}

data TrainScheduleItem = TrainScheduleItem City Time deriving (Eq, Show)

newtype TrainSchedule = TrainSchedule [TrainScheduleItem] deriving (Eq, Show)

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

data TrainState = TrainState TrainPosition Time

stateToPosition (TrainState pos _) = pos 

initialTrainArivingPosition city = cityLeft city - trainWidth
inStationTrainPosition city = fst $ startPoint city
finalTrainLeavingPosition city = cityRight city + trainWidth

createStates (TrainScheduleItem city travelTime) = [
    TrainState (ArivingCity city $ initialTrainArivingPosition city) 0,
    TrainState (TrainInCity city) (5 * second),
    TrainState (LeavingCity city $ inStationTrainPosition city) 0,
    TrainState BetweenCities travelTime]

trainSpeed = 200 / second

trainStateStep :: Time -> [TrainState] -> [TrainState]
trainStateStep delta (TrainState (ArivingCity city trainInCityPosition) _:rest)
    | newPosition < inStationTrainPosition city = 
        TrainState (ArivingCity city newPosition) 0:rest
    | otherwise = trainStateStep restDelta rest
    where newPosition = trainInCityPosition + delta * trainSpeed 
          restDelta = (newPosition - trainInCityPosition) / trainSpeed

trainStateStep delta (TrainState (LeavingCity city trainInCityPosition) _:rest)
    | newPosition < finalTrainLeavingPosition city = 
        TrainState (LeavingCity city newPosition) 0:rest
    | otherwise = trainStateStep restDelta rest
    where newPosition = trainInCityPosition + delta * trainSpeed 
          restDelta = (newPosition - trainInCityPosition) / trainSpeed

trainStateStep delta (TrainState currentPosition remainingTime:rest)
    | newRemainingTime > 0 = TrainState currentPosition newRemainingTime:rest
    | otherwise = trainStateStep (-newRemainingTime) rest
    where newRemainingTime = remainingTime - delta

trainCity :: Signal Time -> TrainSchedule -> SignalGen (Signal TrainPosition)
trainCity deltaSignal (TrainSchedule trainSchedule) = 
    (stateToPosition . head) <~ transfer (cycle $ trainSchedule >>= createStates) trainStateStep deltaSignal

trainWidth = 50

trainRectangleAdjustment (ArivingCity city positionInCity) = Just (positionInCity, snd $ startPoint city)
trainRectangleAdjustment (LeavingCity city positionInCity) = Just (positionInCity, snd $ startPoint city)
trainRectangleAdjustment (TrainInCity city) = Just $ startPoint city
trainRectangleAdjustment _ = Nothing 

cityTrainRectangle (Just city) = Just $  Rectangle (-trainWidth/2, -trainWidth/2) (trainWidth, trainWidth)
cityTrainRectangle _ = Nothing

trainRectangle position = do
    rect <- cityTrainRectangle $ positionCity position
    adj <- trainRectangleAdjustment position
    return $ moveRectangle rect adj
          
someTrains = [Train (TrainSchedule [TrainScheduleItem someCity $ 3 * second, TrainScheduleItem secondCity $ 3 * second]) (TrainId 1)]

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

renderTrain position = rectangleForm blue <$> trainRectangle position

renderTrainByCity :: City -> TrainDescriptor -> Signal Form
renderTrainByCity city trainDescriptor =
    let currentTrainPosition = currentPositionSignal trainDescriptor
        cityCheck position = Just city == positionCity position 
        renderer position = guard (cityCheck position) >> renderTrain position
    in group <$> maybeToList <$> (renderer <$> currentTrainPosition)
    
renderTrainsByCity :: TrainCollection -> City -> Signal Form
renderTrainsByCity trains city =
    let trainFormSignals =  M.elems $ M.map (renderTrainByCity city) (collectionTrains trains)
    in group <$> sequenceA trainFormSignals
