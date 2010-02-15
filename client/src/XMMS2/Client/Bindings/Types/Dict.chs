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

module XMMS2.Client.Bindings.Types.Dict
  ( newDict
  , dictSet
  , DictIter
  , getDictIter
  , dictIterValid
  , dictIterPair
  , dictIterNext
  , dictForeach
  , propdictToDict
  ) where

#include <xmmsclient/xmmsclient.h>
#include <xmms2hs-client.h>

{# context prefix = "xmmsv" #}

import Control.Applicative
import Control.Monad

import XMMS2.Utils
import XMMS2.Client.Exception

{# import XMMS2.Client.Bindings.Types.Value #}


newDict = new_dict >>= takeValue False
{# fun new_dict as new_dict
 {} -> `ValuePtr' id #}

{# fun dict_set as ^
 { withValue*   `Value'
 , withCString* `String'
 , withValue*   `Value'
 } -> `Int' #}


data T = T
{# pointer *xmms2hs_dict_iter_t as DictIterPtr -> T #}
data DictIter = DictIter (ForeignPtr T)

withDictIter (DictIter p) f =
  withForeignPtr p $ \p -> {# get xmms2hs_dict_iter_t->iter #} p >>= f

getDictIter :: Value -> IO DictIter
getDictIter = get TypeDict get_dict_iter (takePtr DictIter finalize_dict_iter)
{# fun xmms2hs_get_dict_iter as get_dict_iter
 { withValue* `Value'
 , alloca-    `DictIterPtr' peek*
 } -> `Bool' #}

foreign import ccall unsafe "&xmms2hs_finalize_dict_iter"
  finalize_dict_iter :: FinalizerPtr T


dictIterPair :: DictIter -> IO (String, Value)
dictIterPair iter = do
  (ok, key, val) <- dict_iter_pair iter
  unless ok $ throwIO $ InvalidIter
  (,) <$> peekCString key <*> takeValue True val
{# fun dict_iter_pair as dict_iter_pair
 { withDictIter* `DictIter'
 , alloca-       `CString'  peek*
 , alloca-       `ValuePtr' peek*
 } -> `Bool' #}

{# fun dict_iter_valid as ^
 { withDictIter* `DictIter'
 } -> `Bool' #}

{# fun dict_iter_next as ^
 { withDictIter* `DictIter'
 } -> `()' #}


type DictForeachFun a = CString -> ValuePtr -> Ptr () -> IO ()
type DictForeachPtr a = FunPtr (DictForeachFun a)

dictForeach :: (String -> Value -> IO ()) -> Value -> IO ()
dictForeach f d = do
  f' <- mkDictForeachPtr $
        \s v _ -> do
          s' <- peekCString s
          v' <- takeValue True v
          f s' v'
  dict_foreach d f' nullPtr
  freeHaskellFunPtr f'
{# fun dict_foreach as dict_foreach
 { withValue* `Value'
 , id         `DictForeachPtr a'
 , id         `Ptr ()'
 } -> `()' #}

foreign import ccall "wrapper"
  mkDictForeachPtr :: DictForeachFun a -> IO (DictForeachPtr a)


propdictToDict :: Value -> [String] -> IO Value
propdictToDict v p = propdict_to_dict v p >>= takeValue False
{# fun propdict_to_dict as propdict_to_dict
 { withValue*         `Value'
 , withCStringArray0* `[String]'
 } -> `ValuePtr' id #}
