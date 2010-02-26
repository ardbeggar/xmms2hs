-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 18 Oct. 2009
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

module XMMS2.Client.Bindings.Stats
  ( PluginType (..)
  , pluginList
  , mainListPlugins
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsc" #}

import XMMS2.Utils

{# import XMMS2.Client.Bindings.Connection #}
{# import XMMS2.Client.Bindings.Result #}


{# enum xmms_plugin_type_t as PluginType
 { underscoreToCase }
 with prefix = "XMMS_"
 deriving (Show) #}


{-# DEPRECATED pluginList "Use mainListPlugins" #-}
pluginList = mainListPlugins

{# fun main_list_plugins as ^
 { withConnection* `Connection'
 , cFromEnum       `PluginType'
 } -> `Result' takeResult* #}
