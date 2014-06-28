module Polar.Types.Input where

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
