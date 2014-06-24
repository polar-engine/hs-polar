module Polar.Types.Options where

import qualified Polar.Types.Color as C
import qualified Polar.Types.Callback as CB

data Options = Options
    { width             :: Int
    , height            :: Int
    , title             :: String
    , swapInterval      :: Int
    , clearColor        :: C.Color
    , keyCB             :: Maybe CB.KeyCB
    , vertexShader      :: String
    , fragmentShader    :: String
    }

defaultOptions :: Options
defaultOptions = Options
    { width             = 1280
    , height            = 720
    , title             = "Polar Engine 4"
    , swapInterval      = 1
    , clearColor        = C.noColor
    , keyCB             = Nothing
    , vertexShader      = "shader"
    , fragmentShader    = "shader"
    }

dimensions :: Options -> (Int, Int)
dimensions opts = (width opts, height opts)
