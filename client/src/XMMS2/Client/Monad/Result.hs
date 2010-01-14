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
  ( Result
  , ResultM
  , ToIO (..)
  , resultRawValue
  , result
  , liftXMMSResult
  , (>>*)
  , handler
  , resultWait
  , resultGetValue
  ) where

import Control.Monad.State
import Control.Monad.Reader  
import Data.Maybe
import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Value
import XMMS2.Client.Result (Result)  
import qualified XMMS2.Client.Result as XR
import Control.Exception


type ResultM m a b = StateT (Maybe a, Value) m b

resultRawValue :: (ValueClass a, MonadXMMS m) => ResultM m a Value
resultRawValue = gets snd

result :: (ValueClass a, MonadXMMS m) => ResultM m a a
result = do
  (res, raw) <- get
  case res of
    Just val ->
      return val
    Nothing  ->
      do val <- lift $ valueGet raw
         put (Just val, raw)
         return val


runResultM ::
  (ValueClass a, Monad m) =>
  ResultM m a b               ->
  Value                       ->
  m b
runResultM f v = evalStateT f (Nothing, v)

class Monad m => ToIO m where
  toIO :: m (m a -> IO a)

instance ToIO IO where
  toIO = return id

instance ToIO m => ToIO (ReaderT r m) where
  toIO = do
    r <- ask
    w <- lift toIO
    return $ w . flip runReaderT r

f >>* h = handler f h
                                
handler ::
  (ValueClass a, ToIO m, MonadIO m) =>
  m (Result a)                ->
  ResultM m a Bool            ->
  m ()
handler f h = do
  result  <- f
  wrapper <- toIO
  liftIO $ XR.resultNotifierSet result $ \v -> wrapper (runResultM h v)

liftXMMSResult = liftXMMS                                    

resultWait = liftIO . XR.resultWait

resultGetValue = liftIO . XR.resultGetValue
