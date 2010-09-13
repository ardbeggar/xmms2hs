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

module XMMS2.Client.Result
  ( Result
  , liftResult
  , resultWait
  , resultGetValue
  , ResultM
  , resultRawValue
  , result
  , resultLength
  , (>>*)
  , persist
  , Default
  , Signal
  , Broadcast
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.ToIO

import XMMS2.Client.Types

import XMMS2.Client.Bindings.Types
import qualified XMMS2.Client.Bindings.Result as B


class PersistentResultClass c where

data Default

data Signal
instance PersistentResultClass Signal

data Broadcast
instance PersistentResultClass Broadcast


newtype Result c a = Result B.Result

liftResult = liftM Result


resultWait (Result r) = B.resultWait r

resultGetValue (Result r) = B.resultGetValue r


newtype RVC c a = RVC { value :: Value }

type ResultM m c a b = ReaderT (RVC c a) (StateT Bool m) b

runResultM :: (MonadIO m, MonadToIO m) => ResultM m c a b -> Value -> m Bool
runResultM f v = execStateT (runReaderT f $ RVC v) False


resultRawValue :: (ValueGet a, MonadIO m, MonadToIO m) => ResultM m c a Value
resultRawValue = value <$> ask

result :: (ValueGet a, MonadIO m, MonadToIO m) => ResultM m c a a
result = resultRawValue >>= liftIO . valueGet

resultLength :: (ValueGet a, ValueGet [a], MonadIO m, MonadToIO m) => ResultM m c [a] Integer
resultLength = resultRawValue >>= liftIO . listGetSize

(>>*) :: (ValueGet a, MonadIO m, MonadToIO m) => m (Result c a) -> ResultM m c a b -> m ()
f >>* h = do
  (Result result)  <- f
  wrapper          <- toIO
  liftIO $ B.resultNotifierSet result $ \v -> wrapper (runResultM h v)

persist :: (Monad m, PersistentResultClass c) => ResultM m c a ()
persist = put True
