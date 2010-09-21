-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 3 Sep. 2009
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

module XMMS2.Client.Bindings.Playlist
  ( PlaylistChangedActions (..)
  , playlistAddURL
  , playlistAddId
  , playlistAddEncoded
  , playlistAddIdlist
  , playlistAddCollection
  , playlistRemoveEntry
  , playlistClear
  , playlistListEntries
  , playlistSort
  , playlistSetNext
  , playlistSetNextRel
  , playlistMoveEntry
  , playlistCurrentPos
  , playlistCurrentActive
  , playlistInsertFull
  , playlistInsertURL
  , playlistInsertId
  , playlistInsertEncoded
  , playlistInsertCollection
  , playlistRAdd
  , playlistRAddEncoded
  , playlistRInsert
  , playlistRInsertEncoded
  , broadcastPlaylistChanged
  , broadcastPlaylistCurrentPos
  , broadcastPlaylistLoaded
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsc" #}

import XMMS2.Utils

{# import XMMS2.Client.Bindings.Types.Value #}
{# import XMMS2.Client.Bindings.Types.Coll #}
{# import XMMS2.Client.Bindings.Connection #}
{# import XMMS2.Client.Bindings.Result #}
{# import XMMS2.Client.Bindings.Coll #}


{# enum xmms_playlist_changed_actions_t as PlaylistChangedActions
 { underscoreToCase
 } with prefix = "XMMS_"
 deriving (Eq, Show) #}


{# fun playlist_add_url as playlistAddURL
 { withConnection*   `Connection'   ,
   withMaybeCString* `Maybe String' ,
   withCString*      `String'
 } -> `Result' takeResult* #}

{# fun playlist_add_id as ^
 { withConnection*   `Connection'
 , withMaybeCString* `Maybe String'
 , cIntConv          `Int32'
 } -> `Result' takeResult* #}

{# fun playlist_add_encoded as ^
 { withConnection*   `Connection'   ,
   withMaybeCString* `Maybe String' ,
   withCString*      `String'
 } -> `Result' takeResult* #}

{# fun playlist_add_idlist as ^
 { withConnection*   `Connection'
 , withMaybeCString* `Maybe String'
 , withColl*         `Coll'
 } -> `Result' takeResult* #}

{# fun playlist_add_collection as ^
 { withConnection*   `Connection'
 , withMaybeCString* `Maybe String'
 , withColl*         `Coll'
 , withValue*        `Value'
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

{# fun playlist_sort as ^
 { withConnection* `Connection'
 , withMaybeCString* `Maybe String'
 , withValue*      `Value'
 } -> `Result' takeResult* #}

{# fun playlist_set_next as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'
 } -> `Result' takeResult* #}

{# fun playlist_set_next_rel as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'
 } -> `Result' takeResult* #}

{# fun playlist_move_entry as ^
 { withConnection*   `Connection'
 , withMaybeCString* `Maybe String'
 , cIntConv          `Int'
 , cIntConv          `Int'
 } -> `Result' takeResult* #}

{# fun playlist_current_pos as ^
 { withConnection*   `Connection' ,
   withMaybeCString* `Maybe String'
 } -> `Result' takeResult* #}

{# fun playlist_current_active as ^
 { withConnection*   `Connection'
 } -> `Result' takeResult* #}

{# fun playlist_insert_full as ^
 { withConnection*   `Connection'
 , withMaybeCString* `Maybe String'
 , cIntConv          `Int'
 , withCString*      `String'
 , withValue*        `Value'
 } -> `Result' takeResult* #}

{# fun playlist_insert_url as playlistInsertURL
 { withConnection*   `Connection'
 , withMaybeCString* `Maybe String'
 , cIntConv          `Int'
 , withCString*      `String'
 } -> `Result' takeResult* #}

{# fun playlist_insert_id as ^
 { withConnection*   `Connection'
 , withMaybeCString* `Maybe String'
 , cIntConv          `Int'
 , cIntConv          `Int32'
 } -> `Result' takeResult* #}

{# fun playlist_insert_encoded as ^
 { withConnection*   `Connection'
 , withMaybeCString* `Maybe String'
 , cIntConv          `Int'
 , withCString*      `String'
 } -> `Result' takeResult* #}

{# fun playlist_insert_collection as ^
 { withConnection*   `Connection'
 , withMaybeCString* `Maybe String'
 , cIntConv          `Int'
 , withColl*         `Coll'
 , withValue*        `Value'
 } -> `Result' takeResult* #}

{# fun playlist_radd as playlistRAdd
 { withConnection*   `Connection'   ,
   withMaybeCString* `Maybe String' ,
   withCString*      `String'
 } -> `Result' takeResult* #}

{# fun playlist_radd_encoded as playlistRAddEncoded
 { withConnection*   `Connection'   ,
   withMaybeCString* `Maybe String' ,
   withCString*      `String'
 } -> `Result' takeResult* #}

{# fun playlist_rinsert as playlistRInsert
 { withConnection*   `Connection'
 , withMaybeCString* `Maybe String'
 , cIntConv          `Int'
 , withCString*      `String'
 } -> `Result' takeResult* #}

{# fun playlist_rinsert_encoded as playlistRInsertEncoded
 { withConnection*   `Connection'
 , withMaybeCString* `Maybe String'
 , cIntConv          `Int'
 , withCString*      `String'
 } -> `Result' takeResult* #}


{# fun broadcast_playlist_changed as ^
 { withConnection*   `Connection'
 } -> `Result' takeResult* #}

{# fun broadcast_playlist_current_pos as ^
 { withConnection*   `Connection'
 } -> `Result' takeResult* #}

{# fun broadcast_playlist_loaded as ^
 { withConnection*   `Connection'
 } -> `Result' takeResult* #}
