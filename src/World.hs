module World where

import Player
import Train
import City
import FRP.Elerea.Simple
import FRP.Helm
import FRP.Helm.Text
import Control.Applicative
import Data.VectorSpace

data World = World (Signal Player) TrainCollection

globalWorld :: SignalGen World
globalWorld = do
    trains <- globalTrainCollection
    player <- Player.player trains
    return $ World player trains

renderWorld :: Signal (Double, Double) -> World -> Signal Form
renderWorld dimensions (World playerSignal _) = renderPlayer <$> dimensions <*> playerSignal

renderPlayer :: (Double, Double) -> Player -> Form
renderPlayer _ (Player _ (OnTrain _)) = toForm $ text $ color white $ toText "Travelling"
renderPlayer dimensions player@(Player position (InCity city)) = viewTransform $ group [
            cityForm city, 
            playerForm player] 
            where viewTransform = move $ negateV position ^-^ viewAddition city position dimensions