{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}

module Polar.Types where

import Control.Lens.TH (makeFields)

import Data.Ratio
import qualified Data.Map as M
import Control.Monad.State
import qualified Graphics.Rendering.OpenGL as GL (Color4(..))
import Graphics.UI.GLFW (KeyCallback)

data Point a = Point { _pointX :: a
                     , _pointY :: a
                     , _pointZ :: a
                     , _pointW :: a
                     } deriving (Eq, Show, Functor, Foldable)
makeFields ''Point

instance Num a => Num (Point a) where
    p1 + p2 = foldr (+) (fromInteger 0) [p1, p2]
    p1 * p2 = foldr (*) (fromInteger 0) [p1, p2]
    p1 - p2 = foldl (-) (fromInteger 0) [p1, p2]
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger x = Point i i i i where i = fromInteger x

defaultPoint :: Num a => Point a
defaultPoint = fromInteger 0

-- TODO: make HasMagnitude somehow using Control.Lens.TH
magnitude :: RealFloat a => Point a -> a
magnitude (Point x y z w) = sqrt (x * x + y * y + z * z + w * w)

data Box a = Box { _boxOrigin :: Point a
                 , _boxSize   :: Point a
                 } deriving (Eq, Show)
makeFields ''Box

defaultBox :: Num a => Box a
defaultBox = Box defaultPoint defaultPoint

data Color = Color3 Double Double Double
           | Color4 Double Double Double Double

data Key = Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9 | Key0
         | KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ
         | KeyK | KeyL | KeyM | KeyN | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT
         | KeyU | KeyV | KeyW | KeyX | KeyY | KeyZ | KeySpace | KeyBacktick
         | KeySemicolon | KeyApostrophe | KeyComma | KeyPeriod
         | KeyDash | KeyEquals | KeySlash | KeyBackslash | KeyBackspace
         | KeyLeftBracket | KeyRightBracket | KeyEnter | KeyTab | KeyEscape
         | KeyInsert | KeyDelete | KeyPageUp | KeyPageDown | KeyHome | KeyEnd
         | KeyUp | KeyLeft | KeyDown | KeyRight | KeyPause | KeyMenu
         | KeyF1 | KeyF2 | KeyF3 | KeyF4 | KeyF5 | KeyF6 | KeyF7 | KeyF8
         | KeyF9 | KeyF10 | KeyF11 | KeyF12
         | KeyUnknown

data KeyAction = KeyDownAction | KeyUpAction

data KeyModifiers = KeyModifiers
    { shiftModifier :: Bool
    , ctrlModifier  :: Bool
    , altModifier   :: Bool
    , superModifier :: Bool
    }

defaultKeyModifiers :: KeyModifiers
defaultKeyModifiers = KeyModifiers
    { shiftModifier = False
    , ctrlModifier  = False
    , altModifier   = False
    , superModifier = False
    }

data KeyEvent = KeyEvent Key KeyAction KeyModifiers

type KeyCB = KeyCallback

{- Event        - event types to be listened for
 - Notification - notification types to be received on events
 -}

data Event = StartupEvent | ShutdownEvent | TickEvent
             deriving (Eq, Ord)

data Notification = StartupNote | ShutdownNote | TickNote (Ratio Integer)

type Listener = Notification -> PolarIO ()

instance Show Listener where show _ = "Listener <native code>"

data Engine = Engine
    { engineTitle     :: String
    , engineStartup   :: PolarIO ()
    , engineListeners :: M.Map Event [Listener]
    , engineWillExit  :: Bool
    , engineViewport  :: Box Int
    }

defaultEngine :: Engine
defaultEngine = Engine
    { engineTitle     = "Polar Engine 4"
    , engineStartup   = return ()
    , engineListeners = M.empty
    , engineWillExit  = False
    , engineViewport  = Box (defaultPoint) (Point 1280 720 0 0)
    }

type Polar = State Engine
type PolarIO = StateT Engine IO

data Options = Options
    { optsWidth          :: Int
    , optsHeight         :: Int
    , optsTitle          :: String
    , optsSwapInterval   :: Int
    , optsClearColor     :: Color
    , optsKeyCB          :: Maybe KeyCB
    , optsVertexShader   :: String
    , optsFragmentShader :: String
    }

defaultOptions :: Options
defaultOptions = Options
    { optsWidth          = 1280
    , optsHeight         = 720
    , optsTitle          = "Polar Engine 4"
    , optsSwapInterval   = 1
    , optsClearColor     = noColor
    , optsKeyCB          = Nothing
    , optsVertexShader   = "shader.vsh"
    , optsFragmentShader = "shader.fsh"
    }

colorToGL :: (Fractional a) => Color -> GL.Color4 a
colorToGL (Color3 r g b) = colorToGL (Color4 r g b 1)
colorToGL (Color4 r g b a) = GL.Color4
    (fromRational (toRational r))
    (fromRational (toRational g))
    (fromRational (toRational b))
    (fromRational (toRational a))

noColor         = Color4 0 0 0 0
blackColor      = Color3 0 0 0
whiteColor      = Color3 1 1 1
redColor        = Color3 1 0 0
greenColor      = Color3 0 1 0
blueColor       = Color3 0 0 1
navyBlueColor   = Color3 0.02 0.05 0.1

mapListeners :: (M.Map Event [Listener] -> M.Map Event [Listener])
             -> Engine -> Engine
mapListeners f v = v {engineListeners = f (engineListeners v)}

setWillExit :: Bool -> Engine -> Engine
setWillExit b v = v {engineWillExit = b}

mapViewport :: (Box Int -> Box Int) -> Engine -> Engine
mapViewport f v = v {engineViewport = f (engineViewport v)}

dimensions :: Options -> (Int, Int)
dimensions opts = (optsWidth opts, optsHeight opts)
