{-# LANGUAGE Safe #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

{-|
  Module      : Polar.Types.Engine
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  This module defines the core engine types.
-}

module Polar.Types.Engine where

import Data.Typeable (Typeable)
import qualified Data.Map as M
import Control.Monad.State
import Polar.Types.Point
import Polar.Types.Box

-- |Alias to 'MonadState' 'Engine' for conveneince.
type MonadPolar = MonadState Engine

-- |Pure engine monad.
type Polar = State Engine

-- |Engine monad transformer
type PolarT = StateT Engine

-- |Hoist 'State' computation into 'StateT m'.
hoistState :: Polar a -> PolarT IO a
hoistState f = StateT (\st -> return (runState f st))

-- |Event listener function.
type Listener a = String -> a -> Polar ()

-- |Wrapper around 'Listener' existentially quantified by the listener argument type.
data ExListener = forall a. Typeable a => ExListener (Listener a)

-- |Engine state environment.
data Engine = Engine { _engineStartup     :: [ExListener]
                     , _engineListeners   :: M.Map String [ExListener]
                     , _engineDeferredIO :: [PolarT IO ()]
                     , _engineWillExit    :: Bool
                     , _engineViewport    :: Box Int
                     }

-- |Default value for 'Engine'.
defaultEngine :: Engine
defaultEngine = Engine
    { _engineStartup     = []
    , _engineListeners   = M.empty
    , _engineDeferredIO = []
    , _engineWillExit    = False
    , _engineViewport    = Box (defaultPoint) (Point 1280 720 0 0)
    }
