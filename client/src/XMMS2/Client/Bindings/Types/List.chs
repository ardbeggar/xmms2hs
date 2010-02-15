-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 14 Feb. 2010
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

module XMMS2.Client.Bindings.Types.List
  ( newList
  , listGetSize
  , listGet
  , listAppend
  , ListIter
  , getListIter
  , listIterValid
  , listIterEntry
  , listIterNext
  ) where

#include <xmmsclient/xmmsclient.h>
#include <xmms2hs-client.h>

{# context prefix = "xmmsv" #}

import Control.Monad

import XMMS2.Utils
import XMMS2.Client.Exception

{# import XMMS2.Client.Bindings.Types.Value #}


newList = new_list >>= takeValue False
{# fun new_list as new_list
 {} -> `ValuePtr' id #}

{# fun list_get_size as ^
 { withValue* `Value'
 } -> `Integer' cIntConv #}

listGet l p = get TypeList ((flip list_get) p) (takeValue True) l
{# fun list_get as list_get
 { withValue* `Value'
 , cIntConv   `Integer'
 , alloca-    `ValuePtr' peek*
 } -> `Bool' #}

{#fun list_append as ^
 { withValue* `Value'
 , withValue* `Value'
 } -> `Int' #}


data T = T
{# pointer *list_iter_t as ListIterPtr -> T #}
data ListIter = ListIter (ForeignPtr T)

withListIter (ListIter p) = withForeignPtr p

getListIter :: Value -> IO ListIter
getListIter = get TypeList get_list_iter (takePtr ListIter finalize_list_iter)
{# fun xmms2hs_get_list_iter as get_list_iter
 { withValue* `Value'
 , alloca-    `ListIterPtr' peek*
 } -> `Bool' #}

foreign import ccall unsafe "&xmms2hs_finalize_list_iter"
  finalize_list_iter :: FinalizerPtr T


listIterEntry iter = do
  (ok, v') <- list_iter_entry iter
  unless ok $ throwIO InvalidIter
  takeValue True v'
{# fun list_iter_entry as list_iter_entry
 { withListIter* `ListIter'
 , alloca-       `ValuePtr' peek*
 } -> `Bool' #}

{# fun list_iter_valid as ^
 { withListIter* `ListIter'
 } -> `Bool' #}

{# fun list_iter_next as ^
 { withListIter* `ListIter'
 } -> `()' #}
