{-# LANGUAGE Safe #-}

module Polar.Types.Core where

import Control.Monad.RWS (RWST)

type CoreEnv = ()
type CoreOutput = ()
data CoreState
type Core = RWST CoreEnv CoreOutput CoreState IO
