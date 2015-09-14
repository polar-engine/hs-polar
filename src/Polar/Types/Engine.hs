{-# LANGUAGE Safe #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module Polar.Types.Engine where

import Data.Typeable (Typeable)
import qualified Data.Map as M
import Control.Monad.State
import Polar.Types.Point
import Polar.Types.Box

type MonadPolar = MonadState Engine
type Polar = State Engine
type PolarIO = StateT Engine IO

type ListenerF a = String -> a -> PolarIO ()
data Listener = forall a. Typeable a => Listener (ListenerF a)

data Engine = Engine { _engineTitle     :: String
                     , _engineStartup   :: [Listener]
                     , _engineListeners :: M.Map String [Listener]
                     , _engineWillExit  :: Bool
                     , _engineViewport  :: Box Int
                     }

defaultEngine :: Engine
defaultEngine = Engine
    { _engineTitle     = "Polar Engine 4"
    , _engineStartup   = []
    , _engineListeners = M.empty
    , _engineWillExit  = False
    , _engineViewport  = Box (defaultPoint) (Point 1280 720 0 0)
    }
