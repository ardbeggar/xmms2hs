-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 4 Sep. 2009
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

module XMMS2.Client.Medialib
  ( medialibGetInfo
  , broadcastMedialibEntryChanged
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsc" #}

import Control.Monad
import XMMS2.Utils
{# import XMMS2.Client.Connection #}
{# import XMMS2.Client.Result #}


{# fun medialib_get_info as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'
 } -> `Result' takeResult* #}


{# fun broadcast_medialib_entry_changed as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}

