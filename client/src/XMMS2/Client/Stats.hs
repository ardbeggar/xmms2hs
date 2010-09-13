-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 15 Feb. 2010
--
--  Copyright (C) 2009-2010 Oleg Belozeorov
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

module XMMS2.Client.Stats
  ( PluginType (..)
  , pluginList
  , mainListPlugins
  ) where

import XMMS2.Client.Types
import XMMS2.Client.Result

import XMMS2.Client.Bindings.Connection
import XMMS2.Client.Bindings.Stats (PluginType (..))
import qualified XMMS2.Client.Bindings.Stats as B


{-# DEPRECATED pluginList "Use mainListPlugins" #-}
pluginList = mainListPlugins

mainListPlugins :: Connection -> PluginType -> IO (Result Default [Dict Data])
mainListPlugins xmmsc ptype =
  liftResult $ B.mainListPlugins xmmsc ptype
