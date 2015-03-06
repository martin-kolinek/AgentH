import FRP.Helm
import FRP.Helm.Time
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import FRP.Elerea.Simple
import Control.Applicative
import World

elements :: Engine -> SignalGen (Signal Element)
elements engine = do
    world <- globalWorld engine
    dims <- Window.dimensions engine
    let worldFormSignal = renderWorld world
    let worldFormsSignal = pure <$> worldFormSignal
    return $ uncurry centeredCollage <$> dims <*> worldFormsSignal

main :: IO ()
main = do
    engine <- startup defaultConfig
    run engine $ elements engine
