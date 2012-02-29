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
  , ResultM (..)
  , ResultHandler
  , (>>*)
  , resultRawValue
  , result
  , resultLength
  , Default
  , Signal
  , Broadcast
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Reader

import XMMS2.Client.Types

import XMMS2.Client.Bindings.Types
import qualified XMMS2.Client.Bindings.Result as B


data Default
data Signal
data Broadcast

newtype Result c a = Result B.Result

liftResult = liftM Result

resultWait (Result r) = B.resultWait r

resultGetValue (Result r) = B.resultGetValue r


newtype ResultM c a b
  = ResultM { unResultM :: ReaderT Value IO b }
  deriving (Functor, Monad, MonadIO)

runResultM :: ResultM c a b -> Value -> IO b
runResultM h = runReaderT (unResultM h)


class ResultHandler c r | c -> r where
  conv :: IO (Result c a) -> (ResultM c a r) -> (r -> Bool)

instance ResultHandler Default () where
  conv _ _ = const False

instance ResultHandler Signal Bool where
  conv _ _ = id

instance ResultHandler Broadcast Bool where
  conv _ _ = id


(>>*) :: ResultHandler c b => IO (Result c a) -> ResultM c a b -> IO ()
f >>* h = do
  (Result result) <- f
  B.resultNotifierSet result $ liftM (conv f h) . runResultM h


resultRawValue :: ResultM c a Value
resultRawValue = ResultM ask

result :: ValueGet a => ResultM c a a
result = resultRawValue >>= liftIO . valueGet

resultLength :: (ValueGet a, ValueGet [a]) => ResultM c [a] Integer
resultLength = resultRawValue >>= liftIO . listGetSize
