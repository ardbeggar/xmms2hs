-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 15 Feb. 2010
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

module XMMS2.Client.Bindings.Types.Bin
  ( Bin
  , withBin
  , makeBin
  , getBin
  , newBin
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsv" #}

import XMMS2.Utils

{# import XMMS2.Client.Bindings.Types.Value #}


data Bin = Bin (Maybe Value) CUInt (ForeignPtr CUChar)

withBin (Bin _ len ptr) f =
  withForeignPtr ptr $ (flip f) len . castPtr

makeBin len =
  takePtr (Bin Nothing len) finalizerFree
    =<< mallocArray (fromIntegral len)


getBin v = do
  (ok, ptr, len) <- get_bin v
  if ok
     then takePtr_ (Bin (Just v) len) ptr
     else raiseGetError TypeBin v
{# fun get_bin as get_bin
 { withValue* `Value'
 , alloca-    `Ptr CUChar' peek*
 , alloca-    `CUInt'      peek*
 } -> `Bool' #}

newBin = (flip withBin) (\ptr len -> new_bin ptr len >>= takeValue True)
{# fun new_bin as new_bin
 { id `Ptr CUChar'
 , id `CUInt'
 } -> `ValuePtr' id #}
