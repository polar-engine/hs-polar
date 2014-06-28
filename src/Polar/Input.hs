module Polar.Input where

import qualified Graphics.UI.GLFW as GLFW
import Polar.Types.Event
import Polar.Types.Input

fromGLFWKeyEvent :: GLFW.Key -> Event
fromGLFWKeyEvent key = KeyEvent (fromGLFWKey key) ????

fromGLFWKey :: GLFW.Key -> Key
fromGLFWKey GLFW.Key'1              = Key1
fromGLFWKey GLFW.Key'2              = Key2
fromGLFWKey GLFW.Key'3              = Key3
fromGLFWKey GLFW.Key'4              = Key4
fromGLFWKey GLFW.Key'5              = Key5
fromGLFWKey GLFW.Key'6              = Key6
fromGLFWKey GLFW.Key'7              = Key7
fromGLFWKey GLFW.Key'8              = Key8
fromGLFWKey GLFW.Key'9              = Key9
fromGLFWKey GLFW.Key'0              = Key0
fromGLFWKey GLFW.Key'A              = KeyA
fromGLFWKey GLFW.Key'B              = KeyB
fromGLFWKey GLFW.Key'C              = KeyC
fromGLFWKey GLFW.Key'D              = KeyD
fromGLFWKey GLFW.Key'E              = KeyE
fromGLFWKey GLFW.Key'F              = KeyF
fromGLFWKey GLFW.Key'G              = KeyG
fromGLFWKey GLFW.Key'H              = KeyH
fromGLFWKey GLFW.Key'I              = KeyI
fromGLFWKey GLFW.Key'J              = KeyJ
fromGLFWKey GLFW.Key'K              = KeyK
fromGLFWKey GLFW.Key'L              = KeyL
fromGLFWKey GLFW.Key'M              = KeyM
fromGLFWKey GLFW.Key'N              = KeyN
fromGLFWKey GLFW.Key'O              = KeyO
fromGLFWKey GLFW.Key'P              = KeyP
fromGLFWKey GLFW.Key'Q              = KeyQ
fromGLFWKey GLFW.Key'R              = KeyR
fromGLFWKey GLFW.Key'S              = KeyS
fromGLFWKey GLFW.Key'T              = KeyT
fromGLFWKey GLFW.Key'U              = KeyU
fromGLFWKey GLFW.Key'V              = KeyV
fromGLFWKey GLFW.Key'W              = KeyW
fromGLFWKey GLFW.Key'X              = KeyX
fromGLFWKey GLFW.Key'Y              = KeyY
fromGLFWKey GLFW.Key'Z              = KeyZ
fromGLFWKey GLFW.Key'Space          = KeySpace
fromGLFWKey GLFW.Key'GraveAccent    = KeyBacktick
fromGLFWKey GLFW.Key'Semicolon      = KeySemicolon
fromGLFWKey GLFW.Key'Apostrophe     = KeyApostrophe
fromGLFWKey GLFW.Key'Comma          = KeyComma
fromGLFWKey GLFW.Key'Period         = KeyPeriod
fromGLFWKey GLFW.Key'Minus          = KeyDash
fromGLFWKey GLFW.Key'Equal          = KeyEquals
fromGLFWKey GLFW.Key'Slash          = KeySlash
fromGLFWKey GLFW.Key'Backslash      = KeyBackslash
fromGLFWKey GLFW.Key'Backspace      = KeyBackspace
fromGLFWKey GLFW.Key'LeftBracket    = KeyLeftBracket
fromGLFWKey GLFW.Key'RightBracket   = KeyRightBracket
fromGLFWKey GLFW.Key'Enter          = KeyEnter
fromGLFWKey GLFW.Key'Tab            = KeyTab
fromGLFWKey GLFW.Key'Escape         = KeyEscape
fromGLFWKey GLFW.Key'Insert         = KeyInsert
fromGLFWKey GLFW.Key'Delete         = KeyDelete
fromGLFWKey GLFW.Key'PageUp         = KeyPageUp
fromGLFWKey GLFW.Key'PageDown       = KeyPageDown
fromGLFWKey GLFW.Key'Home           = KeyHome
fromGLFWKey GLFW.Key'End            = KeyEnd
fromGLFWKey GLFW.Key'Up             = KeyUp
fromGLFWKey GLFW.Key'Left           = KeyLeft
fromGLFWKey GLFW.Key'Down           = KeyDown
fromGLFWKey GLFW.Key'Right          = KeyRight
fromGLFWKey GLFW.Key'Pause          = KeyPause
fromGLFWKey GLFW.Key'Menu           = KeyMenu
fromGLFWKey GLFW.Key'F1             = KeyF1
fromGLFWKey GLFW.Key'F2             = KeyF2
fromGLFWKey GLFW.Key'F3             = KeyF3
fromGLFWKey GLFW.Key'F4             = KeyF4
fromGLFWKey GLFW.Key'F5             = KeyF5
fromGLFWKey GLFW.Key'F6             = KeyF6
fromGLFWKey GLFW.Key'F7             = KeyF7
fromGLFWKey GLFW.Key'F8             = KeyF8
fromGLFWKey GLFW.Key'F9             = KeyF9
fromGLFWKey GLFW.Key'F10            = KeyF10
fromGLFWKey GLFW.Key'F11            = KeyF11
fromGLFWKey GLFW.Key'F12            = KeyF12
fromGLFWKey _                       = KeyUnknown
