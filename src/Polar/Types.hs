module Polar.Types where

import qualified Graphics.Rendering.OpenGL as GL (Color4(..))
import Graphics.UI.GLFW (KeyCallback)

data Point a = Point2 { x :: a, y :: a }
             | Point3 { x :: a, y :: a, z :: a }
              deriving Show

defaultPoint :: Num a => Point a
defaultPoint = Point2 0 0

instance Functor Point where
    fmap f (Point2 x y)   = Point2 (f x) (f y)
    fmap f (Point3 x y z) = Point3 (f x) (f y) (f z)

pointMapBinary :: Num a => (a -> a -> b) -> Point a -> Point a -> Point b
pointMapBinary f (Point2 x1 y1)    (Point2 x2 y2)    = Point2 (f x1 x2) (f y1 y2)
pointMapBinary f (Point3 x1 y1 z1) (Point3 x2 y2 z2) = Point3 (f x1 x2) (f y1 y2) (f z1 z2)
pointMapBinary f (Point2 x1 y1)    p2@(Point3 {})    = pointMapBinary f (Point3 x1 y1 0) p2
pointMapBinary f p1@(Point3 {})    (Point2 x2 y2)    = pointMapBinary f p1 (Point3 x2 y2 0)

instance (Num a, Eq a) => Eq (Point a) where
    Point2 x1 y1    == Point2 x2 y2    = x1 == x2
                                      && y1 == y2
    Point2 x1 y1    == Point3 x2 y2 z2 = x1 == x2
                                      && y1 == y2
                                      && 0  == z2
    Point3 x1 y1 z1 == Point3 x2 y2 z2 = x1 == x2
                                      && y1 == y2
                                      && z1 == z2
    p1@(Point3 {})  == p2@(Point2 {})  = p2 == p1
    p1 /= p2 = not (p1 == p2)

instance Num a => Num (Point a) where
    p1 + p2 = pointMapBinary (+) p1 p2
    p1 * p2 = pointMapBinary (*) p1 p2
    p1 - p2 = pointMapBinary (-) p1 p2
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger i = Point3 i' i' i' where i' = fromInteger i

data Rectangle a = Rectangle
    { origin :: Point a
    , size   :: Point a
    } deriving Show

defaultRectangle :: Num a => Rectangle a
defaultRectangle = Rectangle defaultPoint defaultPoint

instance (Num a, Eq a) => Eq (Rectangle a) where
    Rectangle o1 s1 == Rectangle o2 s2 = o1 == s1 && o2 == s2
    r1 /= r2 = not (r1 == r2)

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

data Event = KeyEvent Key KeyAction KeyModifiers

type KeyCB = KeyCallback

data Engine = Engine
    { title     :: String
    , viewport  :: Rectangle Int
    } deriving Show

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

defaultKeyModifiers :: KeyModifiers
defaultKeyModifiers = KeyModifiers
    { shiftModifier = False
    , ctrlModifier  = False
    , altModifier   = False
    , superModifier = False
    }

defaultEngine :: Engine
defaultEngine = Engine
    { title     = "Polar Engine 4"
    , viewport  = Rectangle (Point2 0 0) (Point2 1280 720)
    }

mapViewport :: (Rectangle Int -> Rectangle Int) -> Engine -> Engine
mapViewport f v = v {viewport = f (viewport v)}

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

dimensions :: Options -> (Int, Int)
dimensions opts = (optsWidth opts, optsHeight opts)

mapX :: (a -> a) -> Point a -> Point a
mapX f v = v {x = f (x v)}

mapY :: (a -> a) -> Point a -> Point a
mapY f v = v {y = f (y v)}

length :: RealFloat a => Point a -> a
length (Point2 x y) = x * cos (atan2 y x)
length (Point3 x y z) = sqrt (x * x + y * y + z * z)

mapOrigin :: (Point a -> Point a) -> Rectangle a -> Rectangle a
mapOrigin f v = v {origin = f (origin v)}

mapSize :: (Point a -> Point a) -> Rectangle a -> Rectangle a
mapSize f v = v {size = f (size v)}
