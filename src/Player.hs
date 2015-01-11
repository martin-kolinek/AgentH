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

playerSpeed = 0.3  

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
                   intersects = isJust $ intersection playerRect $ trainRectangle city
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

renderPlayerSignal :: Signal Player -> Signal (Double, Double) -> Signal Time -> SignalGen (Signal Form)
renderPlayerSignal playerSignal dimensionsSignal timeSignal = do
            let playerCitySignal = playerCity <$> playerSignal
            let playerLocationSignal = playerLocation <$> playerSignal
            let playerPositionSignal = playerPosition <$> playerSignal
            let playerForm = rectangleForm green . playerRectangle <$> playerSignal
            let playerCityForm = group <$> maybeToList <$> fmap cityForm <$> playerCitySignal
            trainCollection <- globalTrainCollection
            let trainForm = renderTrainsByCity trainCollection playerCitySignal
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

