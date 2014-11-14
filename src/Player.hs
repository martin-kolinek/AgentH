{-# LANGUAGE RecursiveDo #-}

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
import Debug.Trace 

data PlayerLocation = InCity City | OnTrain Train deriving (Eq, Show)

data Player = Player {playerPosition :: (Double, Double), playerLocation :: PlayerLocation}

movePlayer :: Player -> (Double, Double) -> Player
movePlayer player@Player {playerPosition = playerPosition, playerLocation = InCity city} dpos = 
    let initVelocity = playerSpeed *^ dpos
        step velocity wall = collide (playerRectangle player) wall velocity 
        finalVelocity = foldl step initVelocity (cityWalls city) 
    in player {playerPosition = playerPosition ^+^ finalVelocity}
movePlayer player _ = player

playerCity Player { playerLocation = InCity city } = Just city
playerCity _ = Nothing

createPlayer city = Player {playerPosition = startPoint city, playerLocation = InCity city}

playerSpeed = 0.3  

playerRadius = 10

playerRectangle player = Rectangle (playerPosition player ^-^ (playerRadius, playerRadius)) (playerRadius * 2, playerRadius * 2)
 
normalizedArrows :: SignalGen (Signal (Double, Double))
normalizedArrows = 
    normalizeArrows <~ arrows ~~ delta
    where 
        normalizeArrows (dx, dy) delta = (realToFrac dx * delta, realToFrac dy * delta)
        
justOnes = do { rec { xs <- Just (1:xs) }
              ; return (map negate xs) }
        
player :: SignalGen (Signal Player)
player = do 
    let tryBoard (Just train) player@Player {playerLocation = InCity city} = 
               let playerRect = playerRectangle player
                   intersects = isJust $ intersection playerRect $ trainRectangle city
               in if intersects then player {playerLocation = OnTrain train} else player
        tryBoard _ player = player 
    deltaSignal <- delta
    arrowsSignal <- normalizedArrows
    rec
        trainLeavingCitySignal <- cityLeavingTrain (playerCity <$> player) deltaSignal 
        let boardedPlayer = tryBoard <$> trainLeavingCitySignal <*> player
        player <- FRP.Elerea.Simple.delay (createPlayer secondCity) $ movePlayer <$> boardedPlayer <*> arrowsSignal  
    return boardedPlayer

renderPlayerSignal :: Signal Player -> Signal (Double, Double) -> Signal Time -> SignalGen (Signal Form)
renderPlayerSignal playerSignal dimensionsSignal timeSignal = do
            let playerCitySignal = playerCity <$> playerSignal
            let playerLocationSignal = playerLocation <$> playerSignal
            let playerPositionSignal = playerPosition <$> playerSignal
            let playerForm = rectangleForm green . playerRectangle <$> playerSignal
            let playerCityForm = group <$> maybeToList <$> fmap cityForm <$> playerCitySignal
            timeSignal <- delta
            trainForm <- renderTrainsByCity someTrains timeSignal playerCitySignal
            combinedForm <- pure $ group <$> sequenceA [playerCityForm, playerForm, trainForm]
            let renderPlayerOutsideCity (OnTrain _) = toForm $ text $ color white $ toText "Travelling"
                renderPlayerOutsideCity _ = group []
                travellingFormSignal = renderPlayerOutsideCity <$> playerLocationSignal
                transformedCombined =  transformRelatively <$> playerCitySignal <*> playerPositionSignal <*> dimensionsSignal <*> combinedForm
            return $ group <$> sequenceA [transformedCombined, travellingFormSignal]
                where transformRelatively (Just playerCity) playerPosition dimensions form = 
                        (move $ negateV playerPosition ^-^ 
                            viewAddition playerCity playerPosition dimensions) form
                      transformRelatively Nothing _ _ _ = group []

