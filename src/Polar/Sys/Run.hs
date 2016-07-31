{-# LANGUAGE Trustworthy #-}

{-|
  Module      : Polar.Sys.Run
  Copyright   : (c) 2015-2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  This module defines functions for running the engine systems abstraction layer.
-}

module Polar.Sys.Run (startupSys, tickSys, shutdownSys) where

import Data.Foldable (traverse_)
import Control.Monad.RWS (get, tell)
import Control.Lens (folded, sequenceOf_)
import Polar.Types
import Polar.Log (logWrite)
import Polar.Logic.Run (tickLogic)

startupSys :: Sys ()
startupSys = do
    sequence_ . map startupSystem =<< use systems

tickSys :: Sys ()
tickSys = do
    (_, _, logicActs) <- runLogic tickLogic () <$> use logicState
    traverse_ runLogicAction logicActs
    sequenceOf_ (systems.folded.tick) =<< get

shutdownSys :: Sys ()
shutdownSys = do
    sequence_ . map shutdownSystem . reverse =<< use systems

startupSystem :: System -> Sys ()
startupSystem s = do
    logWrite DEBUG ("Starting up " ++ s^.name)
    s^.startup

shutdownSystem :: System -> Sys ()
shutdownSystem s = do
    logWrite DEBUG ("Shutting down " ++ s^.name)
    s^.shutdown

runLogicAction :: LogicAction -> Sys ()
runLogicAction LogicExitAction = tell [SysExitAction]
runLogicAction (LogicLogWriteAction priority msg) = logWrite priority msg
