module Player where

import FRP.Helm.Graphics
import FRP.Helm
import FRP.Helm.Keyboard
import FRP.Helm.Time
import FRP.Helm.Utilities
import Data.VectorSpace
import FRP.Elerea.Simple
import City
import Control.Applicative
import Rectangle
import Data.Traversable
import Train
import FRP.Helm.Text
import Data.Maybe 

data Player = Player {playerPosition :: (Double, Double), city :: Maybe City}

movePlayer :: Player -> (Double, Double) -> Player
movePlayer player@Player {playerPosition = playerPosition, city = Just playerCity} dpos = 
    let initVelocity = playerSpeed *^ dpos
        step velocity wall = collide (playerRectangle player) wall velocity 
        finalVelocity = foldl step initVelocity (cityWalls playerCity) 
    in player {playerPosition = playerPosition ^+^ finalVelocity}
movePlayer player _ = player

createPlayer city = Player {playerPosition = startPoint city, city = Just city}

playerSpeed = 0.3  

playerRadius = 10

playerRectangle player = Rectangle (playerPosition player ^-^ (playerRadius, playerRadius)) (playerRadius * 2, playerRadius * 2)

normalizedArrows :: SignalGen (Signal (Double, Double))
normalizedArrows = 
    normalizeArrows <~ arrows ~~ delta
    where 
        normalizeArrows (dx, dy) delta = (realToFrac dx * delta, realToFrac dy * delta)
        
player = foldp (flip movePlayer) (createPlayer secondCity) normalizedArrows

renderPlayerSignal :: Signal Player -> Signal (Double, Double) -> Signal Time -> SignalGen (Signal Form)
renderPlayerSignal playerSignal dimensionsSignal timeSignal = do
            let playerCitySignal = city <$> playerSignal
            let playerPositionSignal = playerPosition <$> playerSignal
            let playerForm = rectangleForm green . playerRectangle <$> playerSignal
            let playerCityForm = group <$> maybeToList <$> fmap cityForm <$> playerCitySignal
            timeSignal <- delta
            trainForm <- renderTrainsByCity someTrains timeSignal playerCitySignal
            combinedForm <- pure $ group <$> sequenceA [playerCityForm, playerForm, trainForm]
            let renderPlayerOutsideCity Nothing = toForm $ text $ color white $ toText "Travelling"
                renderPlayerOutsideCity (Just _) = group []
                travellingFormSignal = renderPlayerOutsideCity <$> playerCitySignal
                transformedCombined =  transformRelatively <$> playerCitySignal <*> playerPositionSignal <*> dimensionsSignal <*> combinedForm
            return $ group <$> sequenceA [transformedCombined, travellingFormSignal]
                where transformRelatively (Just playerCity) playerPosition dimensions form = 
                        (move $ negateV playerPosition ^-^ 
                            viewAddition playerCity playerPosition dimensions) form
                      transformRelatively Nothing _ _ _ = group []

