-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 17 Sep. 2009
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

module XMMS2.Client.Bindings.Coll
  ( collIdlistFromPlaylistFile
  , collSync
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsc" #}

import XMMS2.Utils

{# import XMMS2.Client.Bindings.Connection #}
{# import XMMS2.Client.Bindings.Result #}
{# import XMMS2.Client.Bindings.Types.Coll #}


{# fun coll_idlist_from_playlist_file as ^
 { withConnection* `Connection'
 , withCString*    `String'
 } -> `Result' takeResult* #}

{# fun coll_sync as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}
