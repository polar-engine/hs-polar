{-# LANGUAGE Trustworthy #-}

{-|
  Module      : Polar.Sys.Run
  Copyright   : (c) 2015-2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  This module defines functions for running the engine systems abstraction layer.
-}

module Polar.Sys.Run (tickSys) where

import Data.Foldable (traverse_)
import Control.Monad.RWS (get, tell)
import Control.Lens (use, folded, sequenceOf_)
import Polar.Types
import Polar.Log (logWrite)
import Polar.Logic.Run (tickLogic)

tickSys :: Sys ()
tickSys = do
    (_, _, logicActs) <- runLogic tickLogic () <$> use logicState
    traverse_ runLogicAction logicActs
    sequence_ =<< use tickFunctions
    sequenceOf_ (systems.folded.tick) =<< get

runLogicAction :: LogicAction -> Sys ()
runLogicAction LogicExitAction = tell [SysExitAction]
runLogicAction (LogicLogWriteAction priority msg) = logWrite priority msg
