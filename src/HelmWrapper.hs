module HelmWrapper where

import qualified FRP.Helm.Window as Window
import Control.Applicative

newtype WindowDimensions = WindowDimensions (Double, Double)

windowDimensions engine = (convert <$>) <$> Window.dimensions engine
    where convert (x, y) = WindowDimensions (realToFrac x, realToFrac y)
