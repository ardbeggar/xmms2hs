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

module XMMS2.Client.Bindings.Medialib
  ( medialibAddEntry
  , medialibAddEntryFull
  , medialibAddEntryEncoded
  , medialibGetInfo
  , medialibImportPath
  , medialibImportPathEncoded
  , medialibGetId
  , medialibGetIdEncoded
  , medialibRemoveEntry
  , medialibEntryPropertySetInt
  , medialibEntryPropertySetIntWithSource
  , medialibEntryPropertySetStr
  , medialibEntryPropertySetStrWithSource
  , medialibEntryPropertyRemove
  , medialibEntryPropertyRemoveWithSource
  , xformMediaBrowse
  , broadcastMedialibEntryChanged
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsc" #}

import XMMS2.Utils

{# import XMMS2.Client.Bindings.Types.Value #}
{# import XMMS2.Client.Bindings.Connection #}
{# import XMMS2.Client.Bindings.Result #}


{# fun medialib_add_entry as ^
 { withConnection* `Connection'
 , withCString*    `String'
 } -> `Result' takeResult* #}

{# fun medialib_add_entry_full as ^
 { withConnection* `Connection'
 , withCString*    `String'
 , withValue*      `Value'
 } -> `Result' takeResult* #}

{# fun medialib_add_entry_encoded as ^
 { withConnection* `Connection'
 , withCString*    `String'
 } -> `Result' takeResult* #}

{# fun medialib_get_info as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'
 } -> `Result' takeResult* #}

{# fun medialib_import_path as ^
 { withConnection* `Connection'
 , withCString*    `String'
 } -> `Result' takeResult* #}

{# fun medialib_import_path_encoded as ^
 { withConnection* `Connection'
 , withCString*    `String'
 } -> `Result' takeResult* #}

{# fun medialib_get_id as ^
 { withConnection* `Connection'
 , withCString*    `String'
 } -> `Result' takeResult* #}

{# fun medialib_get_id_encoded as ^
 { withConnection* `Connection'
 , withCString*    `String'
 } -> `Result' takeResult* #}

{# fun medialib_remove_entry as ^
 { withConnection* `Connection'
 , cIntConv        `Int32'
 } -> `Result' takeResult* #}

{# fun medialib_entry_property_set_int as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'      ,
   withCString*    `String'     ,
   cIntConv        `Int32'
 }  -> `Result' takeResult* #}

{# fun medialib_entry_property_set_int_with_source as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'      ,
   withCString*    `String'     ,
   withCString*    `String'     ,
   cIntConv        `Int32'
 }  -> `Result' takeResult* #}

{# fun medialib_entry_property_set_str as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'      ,
   withCString*    `String'     ,
   withCString*    `String'
 }  -> `Result' takeResult* #}

{# fun medialib_entry_property_set_str_with_source as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'      ,
   withCString*    `String'     ,
   withCString*    `String'     ,
   withCString*    `String'
 }  -> `Result' takeResult* #}

{# fun medialib_entry_property_remove as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'      ,
   withCString*    `String'
 }  -> `Result' takeResult* #}

{# fun medialib_entry_property_remove_with_source as ^
 { withConnection* `Connection' ,
   cIntConv        `Int32'      ,
   withCString*    `String'     ,
   withCString*    `String'
 }  -> `Result' takeResult* #}

{# fun xform_media_browse as ^
 { withConnection* `Connection' ,
   withCString*    `String'
 }  -> `Result' takeResult* #}


{# fun broadcast_medialib_entry_changed as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}

