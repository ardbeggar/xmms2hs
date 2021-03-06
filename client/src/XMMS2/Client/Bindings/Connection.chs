-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 1 Sep. 2009
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

module XMMS2.Client.Bindings.Connection
  ( Connection
  , withConnection
  , init
  , connect
  , disconnectCallbackSet
  ) where

#include <xmmsclient/xmmsclient.h>
#include <xmms2hs-client.h>

{# context prefix = "xmmsc" #}

import Prelude hiding (init)

import Control.Applicative
import Control.Monad

import XMMS2.Utils

import XMMS2.Client.Exception


{# pointer *xmmsc_connection_t as Connection foreign newtype #}

takeConnection p
  | p == nullPtr = throwIO ConnectionInitFailed
  | otherwise    = takePtr Connection xmmsc_unref p

foreign import ccall unsafe "&xmmsc_unref"
  xmmsc_unref :: FunPtr (Ptr Connection -> IO ())


{# fun init as ^
 { withCString* `String'
 } -> `Connection' takeConnection* #}

{# fun xmmsc_connect as connect
 { withConnection*   `Connection'
 , withMaybeCString* `Maybe String'
 } -> `Bool' #}

disconnectCallbackSet xmmsc fun = do
  ptr <- mkDisconnectPtr $ const fun
  disconnect_callback_set xmmsc ptr

type DisconnectFun = Ptr () -> IO ()
type DisconnectPtr = FunPtr DisconnectFun

{# fun xmms2hs_disconnect_callback_set as disconnect_callback_set
 { withConnection* `Connection'
 , id              `DisconnectPtr'
 } -> `()' #}

foreign import ccall "wrapper"
  mkDisconnectPtr :: DisconnectFun -> IO DisconnectPtr
