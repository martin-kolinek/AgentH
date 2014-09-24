{-# LANGUAGE TupleSections #-}
module Train where

import FRP.Helm.Time
import FRP.Helm.Graphics
import FRP.Helm.Color
import FRP.Elerea.Simple
import City
import Control.Monad
import Control.Applicative
import Data.Fixed
import Data.Maybe
import Data.Traversable (sequenceA)

data Train = Train { schedule :: [(City, Time)] }

trainCity :: Train -> Signal Time -> SignalGen (Signal (Maybe City))
trainCity train time = undefined 
    --where timeAtCity = 5 * second
          --cityStays = map (const timeAtCity) $ schedule train
          --travelTimes = map snd $ schedule train
          --completeCycle = sum cityStays + sum travelTimes
          --inCurrentCycle = time `mod'` completeCycle*/
          --positions = concat $ transpose [cityStays, travels]

someTrains = [Train [(someCity, 10 * second), (secondCity, 10 * second)]]

renderTrain (Just city) = Just $ move (startPoint city) $ filled blue $ rect 50 50
renderTrain Nothing = Nothing

renderTrainByCity :: Signal Time -> Signal City -> Train -> SignalGen (Signal Form)
renderTrainByCity timeSignal citySignal train = do
    currentTrainCity <- trainCity train timeSignal
    undefined
    
renderTrainsByCity :: [Train] -> Signal Time -> Signal City -> SignalGen (Signal Form)
renderTrainsByCity trains time city = do
    formSignals <- mapM  (renderTrainByCity time city) trains
    return $ group <$> sequenceA formSignals
    