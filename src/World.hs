module World where

import Player
import Train
import City
import FRP.Elerea.Simple
import FRP.Helm
import FRP.Helm.Text
import Control.Applicative
import Data.VectorSpace
import Data.Traversable

data World = World (Signal Player) TrainCollection

globalWorld :: SignalGen World
globalWorld = do
    trains <- globalTrainCollection
    player <- Player.player trains
    return $ World player trains

renderWorld :: Signal (Double, Double) -> World -> Signal Form
renderWorld dimensions (World playerSignal trainCollection) = do
        dims <- dimensions
        player <- playerSignal
        renderPlayer dims trainCollection player

renderPlayer :: (Double, Double) -> TrainCollection -> Player -> Signal Form
renderPlayer _ _ (Player _ (OnTrain _)) = pure $ toForm $ text $ color white $ toText "Travelling"
renderPlayer dimensions trainCollection player@(Player position (InCity city)) =
        let untransformed = group <$> sequenceA [
                                pure $ cityForm city,
                                pure $ playerForm player,
                                renderTrainsByCity trainCollection city
                                ]
            viewTransform = move $ negateV position ^-^ viewAddition city position dimensions
        in viewTransform <$> untransformed