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

class GetConfig a where getConfig :: ConfigProxy a -> SectionName -> OptionName -> PolarCore (Either ConfigError a)

instance {-# OVERLAPPABLE #-} Read a => GetConfig a where getConfig ConfigProxy sect opt = (\cp -> get cp sect opt) <$> use config
instance GetConfig String                           where getConfig ConfigProxy sect opt = (\cp -> get cp sect opt) <$> use config
instance GetConfig Bool                             where getConfig ConfigProxy sect opt = (\cp -> get cp sect opt) <$> use config
