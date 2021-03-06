-- -*-haskell-*-
--  XMMS2 client library — GLib integration.
--
--  Author:  Oleg Belozeorov
--  Created: 2 Sep. 2009
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

module XMMS2.Client.Glib
  ( mainLoopGMainInit
  ) where

#include <xmmsclient/xmmsclient-glib.h>

import C2HS
import XMMS2.Client.Bindings.Connection

mainLoopGMainInit c = withConnection c xmmsc_mainloop_gmain_init

{# fun xmmsc_mainloop_gmain_init as xmmsc_mainloop_gmain_init
 { castPtr `Ptr Connection'
 } -> `Ptr ()' id #}
