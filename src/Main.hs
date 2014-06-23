module Main where

import qualified Polar
import qualified Polar.Types.Color as C
import qualified Polar.Types.Options as O

main :: IO ()
main = Polar.run opts
  where opts = O.defaultOptions { O.clearColor = C.navyBlueColor }

--keyCB :: GLFW.KeyCallback
--keyCB win key scancode action mods = return ()
