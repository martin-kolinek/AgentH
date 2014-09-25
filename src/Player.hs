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

data Player = Player {playerPosition :: (Double, Double), city :: City}

movePlayer :: Player -> (Double, Double) -> Player
movePlayer player dpos = 
    let initVelocity = playerSpeed *^ dpos
        step velocity wall = collide (playerRectangle player) wall velocity 
        finalVelocity = foldl step initVelocity (cityWalls $ city player) 
    in player {playerPosition = playerPosition player ^+^ finalVelocity} 

createPlayer city = Player {playerPosition = startPoint city, city = city}

playerSpeed = 0.3  

playerRadius = 10

playerRectangle player = Rectangle (playerPosition player ^-^ (playerRadius, playerRadius)) (playerRadius * 2, playerRadius * 2)

normalizedArrows :: SignalGen (Signal (Double, Double))
normalizedArrows = 
    normalizeArrows <~ arrows ~~ delta
    where 
        normalizeArrows (dx, dy) delta = (realToFrac dx * delta, realToFrac dy * delta)
        
player = foldp (flip movePlayer) (createPlayer someCity) normalizedArrows

renderPlayerSignal :: Signal Player -> Signal (Double, Double) -> Signal Time -> SignalGen (Signal Form)
renderPlayerSignal playerSignal dimensionsSignal timeSignal = do 
        let playerForm = rectangleForm . playerRectangle <$> playerSignal
        let playerCityForm = cityForm . city <$> playerSignal
        timeSignal <- delta
        trainForm <- renderTrainsByCity someTrains timeSignal (city <$> playerSignal)
        combinedForm <- pure $ group <$> sequenceA [playerForm, playerCityForm, trainForm]
        return $ transformRelatively <$> playerSignal <*> dimensionsSignal <*> combinedForm
            where transformRelatively player dimensions = 
                    move $ negateV playerPosition player ^-^ 
                        viewAddition (city player) (playerPosition player) dimensions
