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
  ( playlistAddURL
  , playlistAddIdlist
  , playlistRemoveEntry
  , playlistClear
  , playlistListEntries
  , playlistSetNext
  , playlistSetNextRel
  , playlistCurrentPos
  , playlistInsertId
  , broadcastPlaylistChanged
  , broadcastPlaylistCurrentPos
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsc" #}         

import XMMS2.Utils
{# import XMMS2.Client.Connection #}
{# import XMMS2.Client.Result #}  
{# import XMMS2.Client.Coll #}  


{# fun playlist_add_url as playlistAddURL
 { withConnection*   `Connection'   ,
   withMaybeCString* `Maybe String' ,
   withCString*      `String'
 } -> `Result' takeResult* #}

{# fun playlist_add_idlist as ^
 { withConnection*   `Connection'   ,
   withMaybeCString* `Maybe String' ,
   withColl*         `Coll'
 } -> `Result' takeResult* #}

{# fun playlist_remove_entry as ^
 { withConnection*   `Connection'
 , withMaybeCString* `Maybe String'
 , cIntConv          `Int'
 } -> `Result' takeResult* #}

{# fun playlist_clear as ^
 { withConnection*   `Connection'   ,
   withMaybeCString* `Maybe String'
 } -> `Result' takeResult* #}

{# fun playlist_list_entries as ^
 { withConnection*   `Connection'   ,
   withMaybeCString* `Maybe String'
 } -> `Result' takeResult* #}

{# fun playlist_set_next as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'
 } -> `Result' takeResult* #}

{# fun playlist_set_next_rel as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'
 } -> `Result' takeResult* #}

{# fun playlist_current_pos as ^
 { withConnection*   `Connection' ,
   withMaybeCString* `Maybe String'
 } -> `Result' takeResult* #}

{# fun playlist_insert_id as ^
 { withConnection*   `Connection'
 , withMaybeCString* `Maybe String'
 , cIntConv          `Int'
 , cIntConv          `Int32'
 } -> `Result' takeResult* #}


{# fun broadcast_playlist_changed as ^
 { withConnection*   `Connection'
 } -> `Result' takeResult* #}

{# fun broadcast_playlist_current_pos as ^
 { withConnection*   `Connection'
 } -> `Result' takeResult* #}
