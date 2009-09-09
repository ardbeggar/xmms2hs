-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 8 Sep. 2009
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

module XMMS2.Client.Monad.Result
  ( Result (..)
  , ResultM
  , liftXMMSResult
  , result
  , resultRawValue
  , handler
  , resultWait
  , resultGetValue
  ) where

import Control.Monad.State
import Data.Maybe
import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Value
import qualified XMMS2.Client.Result as XR


type ResultM a b = StateT (Maybe a, Value) XMMS b

runResultM ::
  ValueTypeClass a =>
  ResultM a b      ->
  Value            ->
  XMMS b
runResultM f v = evalStateT f (Nothing, v)

result :: ValueTypeClass a => ResultM a a
result = do
  (res, raw) <- get
  case res of
    Just val ->
      return val
    Nothing  ->
      do val <- lift $ valueToType raw
         put (Just val, raw)
         return val

resultRawValue :: ValueTypeClass a => ResultM a Value
resultRawValue = gets snd


data (ValueTypeClass a) =>
  Result a = Result XR.Result
                                
handler ::
  ValueTypeClass a =>
  XMMS (Result a)  ->
  ResultM a Bool   ->
  XMMS ()
handler r f = do
  Result r' <- r
  xmmsc <- connection
  liftIO $ XR.resultNotifierSet r' $ runHandler f xmmsc

runHandler f xmmsc v = do
  r <- runXMMS (runResultM f v) xmmsc
  case r of
    Right gr -> return gr
    Left err -> ioError $ userError err
        

liftXMMSResult = liftM Result . liftXMMS                                    


resultWait (Result r) = liftIO $ XR.resultWait r

resultGetValue (Result r) = liftIO $ XR.resultGetValue r
                            