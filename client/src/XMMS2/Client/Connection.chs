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
  , withConnection
  , peekConnection
  , init
  , connect
  ) where

#include <xmmsclient/xmmsclient.h>

import Control.Monad
import Prelude hiding (init)
import XMMS2.Utils  


{# pointer *xmmsc_connection_t as Connection foreign newtype #}

peekConnection p = liftM Connection $ newForeignPtr xmmsc_unref p
foreign import ccall unsafe "&xmmsc_unref"
  xmmsc_unref :: FunPtr (Ptr Connection -> IO ())


{# fun xmmsc_init as init
 { withCString* `String'
 } -> `Maybe Connection' maybeConnection* #}

{# fun xmmsc_connect as connect
 { withConnection*   `Connection'   ,
   withMaybeCString* `Maybe String'
 } -> `Bool' #}


maybeConnection p
  | p == nullPtr = return Nothing
  | otherwise    = liftM Just $ peekConnection p
