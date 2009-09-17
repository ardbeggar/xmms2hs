-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 17 Sep. 2009
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

module XMMS2.Client.CollBase
  ( CollPtr
  , Coll
  , withColl
  , takeColl
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsc" #}         

import XMMS2.Utils


data Xmmsv_coll_t = Xmmsv_coll_t
{# pointer *xmmsv_coll_t as CollPtr -> Xmmsv_coll_t #}
data Coll = forall a. Coll (Maybe a) (ForeignPtr Xmmsv_coll_t)

withColl (Coll _ p) = withForeignPtr p

takeColl o p = do
  f <- maybe (newForeignPtr xmmsv_coll_unref p) (\_ -> newForeignPtr_ p) o
  return $ Coll o f
foreign import ccall unsafe "&xmmsv_coll_unref"
  xmmsv_coll_unref :: FunPtr (CollPtr -> IO ())
