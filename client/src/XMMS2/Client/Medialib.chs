-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 4 Sep. 2009
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

module XMMS2.Client.Medialib
  ( medialibAddEntry
  , medialibAddEntryFull
  , medialibAddEntryEncoded
  , medialibGetInfo
  , medialibGetId
  , medialibGetIdEncoded
  , medialibEntryPropertySetInt
  , medialibEntryPropertySetIntWithSource
  , medialibEntryPropertySetStr
  , medialibEntryPropertySetStrWithSource
  , medialibEntryPropertyRemove
  , medialibEntryPropertyRemoveWithSource
  , BrowseEntry (..)
  , xformMediaBrowse
  , broadcastMedialibEntryChanged
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsc" #}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import XMMS2.Utils

{# import XMMS2.Client.Connection #}
{# import XMMS2.Client.Result #}
{# import XMMS2.Client.Value #}


{# fun medialib_add_entry as ^
 { withConnection* `Connection'
 , withCString*    `String'
 } -> `Result ()' takeResult* #}

medialibAddEntryFull xmmsc url args =
  valueNew args >>= medialib_add_entry_full xmmsc url
{# fun medialib_add_entry_full as medialib_add_entry_full
 { withConnection* `Connection'
 , withCString*    `String'
 , withValue*      `Value'
 } -> `Result ()' takeResult* #}

{# fun medialib_add_entry_encoded as ^
 { withConnection* `Connection'
 , withCString*    `String'
 } -> `Result ()' takeResult* #}

{# fun medialib_get_info as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'
 } -> `Result PropDict' takeResult* #}

{# fun medialib_get_id as ^
 { withConnection* `Connection'
 , withCString*    `String'
 } -> `Result Int32' takeResult* #}

{# fun medialib_get_id_encoded as ^
 { withConnection* `Connection'
 , withCString*    `String'
 } -> `Result Int32' takeResult* #}

{# fun medialib_entry_property_set_int as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'      ,
   withCString*    `String'     ,
   cIntConv        `Int32'
 }  -> `Result ()' takeResult* #}

{# fun medialib_entry_property_set_int_with_source as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'      ,
   withCString*    `String'     ,
   withCString*    `String'     ,
   cIntConv        `Int32'
 }  -> `Result ()' takeResult* #}

{# fun medialib_entry_property_set_str as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'      ,
   withCString*    `String'     ,
   withCString*    `String'
 }  -> `Result ()' takeResult* #}

{# fun medialib_entry_property_set_str_with_source as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'      ,
   withCString*    `String'     ,
   withCString*    `String'     ,
   withCString*    `String'
 }  -> `Result ()' takeResult* #}

{# fun medialib_entry_property_remove as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'      ,
   withCString*    `String'
 }  -> `Result ()' takeResult* #}

{# fun medialib_entry_property_remove_with_source as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'      ,
   withCString*    `String'     ,
   withCString*    `String'
 }  -> `Result ()' takeResult* #}


data BrowseEntry
  = BrowseEntry { entryPath  :: String
                , entryIsDir :: Bool }

instance ValueGet BrowseEntry where
  valueGet v = do
    dict <- valueGet v
    maybe (fail "not a browse entry") return $ do
      path <- lookupString "realpath" dict `mplus` lookupString "path" dict
      dirp <- (1 ==) <$> lookupInt32 "isdir" dict
      return BrowseEntry { entryPath = path, entryIsDir = dirp }

{# fun xform_media_browse as ^
 { withConnection* `Connection' ,
   withCString*    `String'
 }  -> `Result [BrowseEntry]' takeResult* #}


{# fun broadcast_medialib_entry_changed as ^
 { withConnection* `Connection'
 } -> `Result Int32' takeResult* #}

