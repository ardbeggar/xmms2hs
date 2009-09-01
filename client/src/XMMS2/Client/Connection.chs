-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 1 Sep. 2009
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

module XMMS2.Client.Connection
  ( Connection
  , init
  , connect
  ) where

#include <xmmsclient/xmmsclient.h>

import C2HS         
import Prelude hiding (init)


{# pointer *xmmsc_connection_t as Connection newtype #}

{# fun xmmsc_init as init
 { withCString* `String'
 } -> `Maybe Connection' maybeConnection #}

{# fun xmmsc_connect as connect
 { id                `Connection'   ,
   withMaybeCString* `Maybe String'
 } -> `Bool' #}

maybeConnection (Connection p) = nothingIf (== nullPtr) Connection p

withMaybeCString (Just s) f = withCString s f
withMaybeCString Nothing f  = f nullPtr
                  