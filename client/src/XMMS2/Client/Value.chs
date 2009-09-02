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

module XMMS2.Client.Value
  ( Value
  , withValue
  , peekValue
  , valueRef
  , getInt
  , Int32
  ) where

#include <xmmsclient/xmmsclient.h>

import C2HS         
import Control.Monad
import Data.Int (Int32)
  

{# pointer *xmmsv_t as Value foreign newtype #}

peekValue p = liftM Value $ newForeignPtr xmmsv_unref p
foreign import ccall unsafe "&xmmsv_unref"
  xmmsv_unref :: FunPtr (Ptr Value -> IO ())

{# fun xmmsv_ref as valueRef
 { withValue* `Value'
 } -> `()' #}

getInt = toMaybe . xmmsv_get_int

{# fun xmmsv_get_int as xmmsv_get_int
 { withValue* `Value'                ,
   alloca-    `Int32'   peekIntConv*
 } -> `Bool' #}


toMaybe = liftM $ \(r, v) -> if r then Just v else Nothing
