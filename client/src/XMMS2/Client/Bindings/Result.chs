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

module XMMS2.Client.Bindings.Result
  ( Result
  , ResultPtr
  , withResult
  , takeResult
  , resultWait
  , resultGetValue
  , ResultNotifier
  , resultNotifierSet
  , resultCallbackSet
  ) where

#include <xmmsclient/xmmsclient.h>
#include <xmms2hs-client.h>

{# context prefix = "xmmsc" #}

import Control.Monad

import XMMS2.Utils

{# import XMMS2.Client.Bindings.Types.Value #}


data T = T
{# pointer *result_t as ResultPtr -> T #}
data Result = Result (ForeignPtr T)

withResult (Result p) = withForeignPtr p

takeResult = takePtr Result xmmsc_result_unref

foreign import ccall unsafe "&xmmsc_result_unref"
  xmmsc_result_unref :: FunPtr (ResultPtr -> IO ())


resultGetValue :: Result -> IO Value
resultGetValue r = result_get_value r >>= takeValue True
{# fun result_get_value as result_get_value
 { withResult* `Result'
 } -> `ValuePtr' id #}

{# fun result_wait as ^
 { withResult* `Result'
 } -> `()' #}


type ResultNotifier = Value -> IO Bool

resultNotifierSet :: Result -> ResultNotifier -> IO ()
resultNotifierSet r f = do
  n <- mkNotifierPtr $ \p _ -> takeValue True p >>= liftM fromBool . f
  xmms2hs_result_notifier_set r n

resultCallbackSet o f = do
  r <- o
  resultNotifierSet r f


type NotifierFun = ValuePtr -> Ptr () -> IO CInt
type NotifierPtr = FunPtr NotifierFun

{# fun xmms2hs_result_notifier_set as xmms2hs_result_notifier_set
 { withResult* `Result'
 , id          `NotifierPtr'
 } -> `()' #}

foreign import ccall "wrapper"
  mkNotifierPtr :: NotifierFun -> IO NotifierPtr