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
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.ToIO

import XMMS2.Client.Types
import XMMS2.Client.Bindings.Types
import qualified XMMS2.Client.Bindings.Result as B


newtype Result a = Result B.Result

liftResult = (return . Result =<<)


resultWait (Result r) = B.resultWait r

resultGetValue (Result r) = B.resultGetValue r


newtype RVC a = RVC { value :: Value }

type ResultM m a b = ReaderT (RVC a) m b

runResultM :: (MonadIO m, MonadToIO m) => ResultM m a b -> Value -> m b
runResultM f v = runReaderT f $ RVC v


resultRawValue :: (ValueGet a, MonadIO m, MonadToIO m) => ResultM m a Value
resultRawValue = value <$> ask

result :: (ValueGet a, MonadIO m, MonadToIO m) => ResultM m a a
result = resultRawValue >>= liftIO . valueGet

resultLength :: (ValueGet a, ValueGet [a], MonadIO m, MonadToIO m) => ResultM m [a] Integer
resultLength = resultRawValue >>= liftIO . listGetSize


(>>*) :: (ValueGet a, MonadIO m, MonadToIO m) => m (Result a) -> ResultM m a Bool -> m ()
f >>* h = do
  (Result result)  <- f
  wrapper          <- toIO
  liftIO $ B.resultNotifierSet result $ \v -> wrapper (runResultM h v)
