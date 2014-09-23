import FRP.Helm
import FRP.Helm.Time
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import FRP.Elerea.Simple
import City
import Player
import Control.Applicative

intsToDoubles (x, y) = (realToFrac x, realToFrac y)

elements :: Engine -> SignalGen (Signal Element)
elements engine = do
    playerSignal <- player
    dims <- Window.dimensions engine
    timeSignal <- running
    let intDims = intsToDoubles <$> dims 
    form <- renderPlayerSignal playerSignal intDims timeSignal 
    return $ uncurry centeredCollage <$> dims <*> (pure <$> form)

main :: IO ()
main = do
    engine <- startup defaultConfig
    run engine $ elements engine
