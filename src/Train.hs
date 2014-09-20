{-# LANGUAGE TupleSections #-}
module Train where

import FRP.Helm.Time
import FRP.Helm.Graphics
import FRP.Helm.Color
import FRP.Elerea.Simple
import City
import Control.Monad
import Data.List

data Train = Train { schedule :: [(City, Time)] }

trainCity :: Train -> Time -> Maybe City 
trainCity train time = undefined 
    where timeAtCity = 5 * second
          createCityTime time city = (Just city, time)
          cityStays = map (createCityTime timeAtCity . fst) $ schedule train
          travels = map ((Nothing, ) . snd) $ schedule train
          positions = concat $ transpose [cityStays, travels]

someTrains = [Train [(someCity, 10 * second), (secondCity, 10 * second)]]

renderTrain train time city = guard (Just city == trainCity train time) >>
    (Just $ move (startPoint city) $ filled blue $ rect 50 50)