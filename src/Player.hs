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

data Player = Player {position :: (Double, Double), city :: City}

movePlayer player dpos = player {position = position player ^+^ (playerSpeed *^ dpos)}

createPlayer city = Player {position = startPoint city, city = city}

playerSpeed = 0.3  

playerForm = filled white $ square 20

normalizedArrows :: SignalGen (Signal (Double, Double))
normalizedArrows = 
    normalizeArrows <~ arrows ~~ delta
    where 
        normalizeArrows (dx, dy) delta = (realToFrac dx * delta, realToFrac dy * delta)
        
player = foldp (flip movePlayer) (createPlayer someCity) normalizedArrows

renderPlayer :: Signal Player -> Signal Form
renderPlayer player = renderPlayerRaw <$> player
    where renderPlayerRaw player = group [
            renderCityInPos (city player) (position player),
            filled green $ square 10
            ]
          renderCityInPos city position = transformRelatively position $ cityForm city
          transformRelatively (x, y) = move (-x, -y)