module Main where

import qualified Polar
import qualified Polar.Types.Options as O

main :: IO ()
main = Polar.run O.defaultOptions

--keyCB :: GLFW.KeyCallback
--keyCB win key scancode action mods = return ()
