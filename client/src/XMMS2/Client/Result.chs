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

module XMMS2.Client.Result
  ( Result
  , withResult
  , peekResult
  , getValue
  , wait
  ) where

#include <xmmsclient/xmmsclient.h>
#include <xmms2hs-client.h>

import C2HS
import Control.Monad

{# import XMMS2.Client.Value #}


{# pointer *xmmsc_result_t as Result foreign newtype #}

peekResult p = liftM Result $ newForeignPtr xmmsc_result_unref p
foreign import ccall unsafe "&xmmsc_result_unref"
  xmmsc_result_unref :: FunPtr (Ptr Result -> IO ())
                        

{# fun xmmsc_result_get_value as getValue
 { withResult* `Result'
 } -> `Value' peekValue* #}

{# fun xmmsc_result_wait as wait
 { withResult* `Result'
 } -> `()' #}
