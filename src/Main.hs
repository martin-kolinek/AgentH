import FRP.Helm
import FRP.Helm.Time
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import FRP.Elerea.Simple
import City
import Player
import Control.Applicative

elements :: Engine -> SignalGen (Signal Element)
elements engine = do
    playerSignal <- player
    let form = (:[]) <$> renderPlayer playerSignal
    dims <- Window.dimensions engine
    return $ uncurry centeredCollage <$> dims <*> form

main :: IO ()
main = do
    engine <- startup defaultConfig
    run engine $ elements engine
