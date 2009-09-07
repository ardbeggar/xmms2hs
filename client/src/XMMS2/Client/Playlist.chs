-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 3 Sep. 2009
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

module XMMS2.Client.Playlist
  ( listEntries
  , setNext
  , broadcastPlaylistChanged
  ) where

#include <xmmsclient/xmmsclient.h>

import XMMS2.Utils
{# import XMMS2.Client.Connection #}
{# import XMMS2.Client.Result #}  


{# fun xmmsc_playlist_list_entries as listEntries
 { withConnection*   `Connection'   ,
   withMaybeCString* `Maybe String'
 } -> `Result' peekResult* #}

{# fun xmmsc_playlist_set_next as setNext
 { withConnection* `Connection' ,
   cIntConv        `Integer'
 } -> `Result' peekResult* #}

{# fun xmmsc_broadcast_playlist_changed as broadcastPlaylistChanged
 { withConnection*   `Connection'
 } -> `Result' peekResult* #}
