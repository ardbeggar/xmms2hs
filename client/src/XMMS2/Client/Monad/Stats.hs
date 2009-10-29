-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 18 Oct. 2009
--
--  Copyright (C) 2009 Oleg Belozeorov
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 3 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--

module XMMS2.Client.Monad.Stats
  ( PluginType (..)
  , pluginList
  ) where

import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Value
import XMMS2.Client.Monad.Result
import XMMS2.Client.Stats (PluginType (..))
import qualified XMMS2.Client.Stats as XS


pluginList :: MonadXMMS m => PluginType -> m (Result [Dict (ValueData Immutable)])
pluginList pType = liftXMMSResult $ \xmmsc -> XS.pluginList xmmsc pType
