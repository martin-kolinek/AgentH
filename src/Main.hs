import FRP.Helm
import FRP.Helm.Time
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import FRP.Elerea.Simple
import Control.Applicative
import World

intsToDoubles (x, y) = (realToFrac x, realToFrac y)

elements :: Engine -> SignalGen (Signal Element)
elements engine = do
    world <- globalWorld
    dims <- Window.dimensions engine
    let worldFormSignal = pure <$> renderWorld (intsToDoubles <$> dims) world
    return $ uncurry centeredCollage <$> dims <*> worldFormSignal

main :: IO ()
main = do
    engine <- startup defaultConfig
    run engine $ elements engine
