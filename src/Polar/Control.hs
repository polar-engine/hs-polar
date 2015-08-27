module Polar.Control where

import Control.Monad.State (modify)
import Control.Lens ((.=))
import Polar.Types

exit :: PolarIO ()
exit = willExit .= True
