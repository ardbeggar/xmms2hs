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
  , takeResult
  , resultGetValue
  , resultWait
  , ResultNotifier
  , resultNotifierSet
  , resultCallbackSet
  ) where

#include <xmmsclient/xmmsclient.h>
#include <xmms2hs-client.h>

{# context prefix = "xmmsc" #}

import Control.Monad
import XMMS2.Utils
{# import XMMS2.Client.Value #}


{# pointer *xmmsc_result_t as Result foreign newtype #}

takeResult p = liftM Result $ newForeignPtr xmmsc_result_unref p
foreign import ccall unsafe "&xmmsc_result_unref"
  xmmsc_result_unref :: FunPtr (Ptr Result -> IO ())
                        

resultGetValue r = result_get_value r >>= takeValue (Just r)
{# fun result_get_value as result_get_value
 { withResult* `Result'
 } -> `ValuePtr' id #}

{# fun result_wait as ^
 { withResult* `Result'
 } -> `()' #}


type ResultNotifier = Value -> IO Bool

resultNotifierSet :: Result -> ResultNotifier -> IO ()
resultNotifierSet r f = do
  n <- mkNotifierPtr $ \p _ -> takeValue (Just r) p >>= liftM fromBool . f
  xmms2hs_result_notifier_set r n

resultCallbackSet o f = do
  r <- o
  resultNotifierSet r f


type NotifierFun = ValuePtr -> Ptr () -> IO CInt
type NotifierPtr = FunPtr NotifierFun
  
{# fun xmms2hs_result_notifier_set as xmms2hs_result_notifier_set
 { withResult* `Result'      ,
   id          `NotifierPtr'
 } -> `()' #}

foreign import ccall "wrapper"
  mkNotifierPtr :: NotifierFun -> IO NotifierPtr
