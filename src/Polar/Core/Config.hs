{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
  Module      : Polar.Core.Config
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Engine configuration functions.
-}

module Polar.Core.Config (getConfig) where

import Control.Lens.Getter (use)
import Polar.ConfigFile
import Polar.Types
import {-# SOURCE #-} Polar.Core.Log (logFatal)

class GetConfig a where getConfig :: ConfigProxy a -> SectionName -> OptionName -> PolarCore a

instance {-# OVERLAPPABLE #-} Read a => GetConfig a where getConfig _ s o = (\c -> get c s o) <$> use config >>= forceOption
instance GetConfig String                           where getConfig _ s o = (\c -> get c s o) <$> use config >>= forceOption
instance GetConfig Bool                             where getConfig _ s o = (\c -> get c s o) <$> use config >>= forceOption

forceOption :: Either ConfigError a -> PolarCore a
forceOption (Left (err, _)) = logFatal ("failed to get config option (" ++ show err ++ ")")
forceOption (Right x) = return x
