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

module XMMS2.Client.Result
  ( Result
  , ResultPtr
  , withResult
  , takeResult
  , resultGetValue
  , resultWait
  , ResultNotifier
  , resultNotifierSet
  , resultCallbackSet
  , ResultM
  , resultRawValue
  , result
  , resultLength
  , (>>*)
  ) where

#include <xmmsclient/xmmsclient.h>
#include <xmms2hs-client.h>

{# context prefix = "xmmsc" #}

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ToIO

import XMMS2.Utils
import XMMS2.Client.Monad.Monad

{# import XMMS2.Client.Bindings.Types.Value #}
{# import XMMS2.Client.Value #}


data T = T
{# pointer *result_t as ResultPtr -> T #}
data Result a = Result (ForeignPtr T)

withResult (Result p) = withForeignPtr p

takeResult = takePtr Result xmmsc_result_unref

foreign import ccall unsafe "&xmmsc_result_unref"
  xmmsc_result_unref :: FunPtr (ResultPtr -> IO ())


resultGetValue :: Result a -> IO Value
resultGetValue r = result_get_value r >>= takeValue True
{# fun result_get_value as result_get_value
 { withResult* `Result a'
 } -> `ValuePtr' id #}

{# fun result_wait as ^
 { withResult* `Result a'
 } -> `()' #}


type ResultNotifier = Value -> IO Bool

resultNotifierSet :: Result a -> ResultNotifier -> IO ()
resultNotifierSet r f = do
  n <- mkNotifierPtr $ \p _ -> takeValue True p >>= liftM fromBool . f
  xmms2hs_result_notifier_set r n

resultCallbackSet o f = do
  r <- o
  resultNotifierSet r f


type NotifierFun = ValuePtr -> Ptr () -> IO CInt
type NotifierPtr = FunPtr NotifierFun

{# fun xmms2hs_result_notifier_set as xmms2hs_result_notifier_set
 { withResult* `Result a'
 , id          `NotifierPtr'
 } -> `()' #}

foreign import ccall "wrapper"
  mkNotifierPtr :: NotifierFun -> IO NotifierPtr


newtype RVC a = RVC { value :: Value }

type ResultM m a b = ReaderT (RVC a) m b

runResultM :: XMMSM m => ResultM m a b -> Value -> m b
runResultM f v = runReaderT f $ RVC v


resultRawValue :: (ValueGet a, XMMSM m) => ResultM m a Value
resultRawValue = value <$> ask

result :: (ValueGet a, XMMSM m) => ResultM m a a
result = resultRawValue >>= lift . valueGet

resultLength :: (ValueGet a, ValueGet [a], XMMSM m) => ResultM m [a] Integer
resultLength = resultRawValue >>= liftIO . listGetSize


(>>*) :: (ValueGet a, XMMSM m) => m (Result a) -> ResultM m a Bool -> m ()
f >>* h = do
  result  <- f
  wrapper <- toIO
  liftIO $ resultNotifierSet result $ \v -> wrapper (runResultM h v)
