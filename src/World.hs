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
import qualified HelmWrapper (windowDimensions)
import HelmWrapper (WindowDimensions)

data World = World (Signal Player) TrainCollection (Signal WindowDimensions)

globalWorld :: Engine -> SignalGen World
globalWorld engine = do
    trains <- globalTrainCollection
    player <- Player.player trains
    winDims <- HelmWrapper.windowDimensions engine
    return $ World player trains winDims

renderWorld :: World -> Signal Form
renderWorld (World playerSignal trainCollection dimensions) = do
        dims <- dimensions
        player <- playerSignal
        renderPlayer dims trainCollection player

renderPlayer :: WindowDimensions -> TrainCollection -> Player -> Signal Form
renderPlayer _ _ (Player _ (OnTrain _)) = pure $ toForm $ text $ color white $ toText "Travelling"
renderPlayer dimensions trainCollection player@(Player position (InCity city)) =
        let untransformed = group <$> sequenceA [
                                pure $ cityForm city,
                                pure $ playerForm player,
                                renderTrainsByCity trainCollection city
                                ]
            viewTransform = move $ negateV position ^-^ viewAddition city position dimensions
        in viewTransform <$> untransformed