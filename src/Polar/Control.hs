{-# LANGUAGE Safe #-}

module Polar.Control where

import Control.Lens ((.=))
import Polar.Types

exit :: PolarIO ()
exit = willExit .= True
