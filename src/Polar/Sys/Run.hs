{-# LANGUAGE Safe #-}

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
import Control.Monad.RWS (tell)
import Control.Lens.Getter (use)
import Polar.Types
import Polar.Log (logWrite)
import Polar.Logic.Run (tickLogic)

tickSys :: Sys ()
tickSys = do
    (_, _, logicActs) <- runLogic tickLogic () <$> use logicState
    traverse_ runLogicAction logicActs
    sequence_ =<< use tickFunctions

runLogicAction :: LogicAction -> Sys ()
runLogicAction LogicExitAction = tell [SysExitAction]
runLogicAction (LogicLogWriteAction priority msg) = logWrite priority msg
