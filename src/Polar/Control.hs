module Polar.Control where

import Control.Monad.State (modify)
import Polar.Types

exit :: PolarIO ()
exit = modify (setWillExit True)
