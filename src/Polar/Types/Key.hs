{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Types.Key
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  This module defines keyboard data types.
-}

module Polar.Types.Key where

-- |Wrapper type for keyboard events.
data KeyEvent = KeyEvent Key KeyAction KeyModifiers

-- |Sources of keyboard input.
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

-- |Keyboard actions.
data KeyAction = KeyDownAction | KeyUpAction

-- |Keyboard modifiers.
data KeyModifiers = KeyModifiers
    { shiftModifier :: Bool
    , ctrlModifier  :: Bool
    , altModifier   :: Bool
    , superModifier :: Bool
    }

-- |Default value for 'KeyModifiers'
defaultKeyModifiers :: KeyModifiers
defaultKeyModifiers = KeyModifiers False False False False
