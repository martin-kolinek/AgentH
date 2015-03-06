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
import Control.Monad (liftM)
import Rectangle
import Data.Traversable
import Train
import FRP.Helm.Text
import Data.Maybe
import Debug.Trace 

data PlayerLocation = InCity City | OnTrain TrainId deriving (Eq, Show)

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

playerTrain Player { playerLocation = OnTrain train } = Just train
playerTrain _ = Nothing

createPlayer city = Player {playerPosition = startPoint city, playerLocation = InCity city}

playerSpeed = 160 / second

playerRadius = 10

playerRectangle player = Rectangle (playerPosition player ^-^ (playerRadius, playerRadius)) (playerRadius * 2, playerRadius * 2)
 
normalizedArrows :: SignalGen (Signal (Double, Double))
normalizedArrows = 
    normalizeArrows <~ arrows ~~ delta
    where 
        normalizeArrows (dx, dy) delta = (realToFrac dx * delta, realToFrac dy * delta)
        
player :: TrainCollection -> SignalGen (Signal Player)
player trainCollection = mdo 
    let tryBoard (Just train) player@Player {playerLocation = InCity city} = 
               let playerRect = playerRectangle player
                   intersects = isJust $ intersection playerRect =<< trainRectangle (TrainInCity city)
               in if intersects then player {playerLocation = OnTrain train} else player
        tryBoard _ player = player
        tryUnboard _ (Just city) = createPlayer city
        tryUnboard player _ = player
    deltaSignal <- delta
    arrowsSignal <- normalizedArrows
    let trainLeavingCitySignal = cityLeavingTrain trainCollection (playerCity <$> player)
    let trainComingSignal = trainComing trainCollection (playerTrain <$> player)
    let boardedPlayer = tryBoard <$> trainLeavingCitySignal <*> player
    let unboardedPlayer = tryUnboard <$> boardedPlayer <*> trainComingSignal
    player <- FRP.Elerea.Simple.delay (createPlayer secondCity) $ movePlayer <$> unboardedPlayer <*> arrowsSignal  
    return boardedPlayer

playerForm player = rectangleForm green . playerRectangle $ player
