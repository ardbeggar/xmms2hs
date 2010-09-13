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
  , (>>*)
  , resultRawValue
  , result
  , resultLength
  , Default
  , Signal
  , Broadcast
  , persist
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State

import XMMS2.Client.Types

import XMMS2.Client.Bindings.Types
import qualified XMMS2.Client.Bindings.Result as B


newtype Result c a = Result B.Result

liftResult = liftM Result

resultWait (Result r) = B.resultWait r

resultGetValue (Result r) = B.resultGetValue r


newtype ResultM c a b
  = ResultM { unResultM :: ReaderT Value (StateT Bool IO) b }
  deriving (Functor, Monad, MonadIO)

execResultM :: ResultM c a b -> Value -> IO Bool
execResultM r v = execStateT (runReaderT (unResultM r) v) False


(>>*) :: IO (Result c a) -> ResultM c a b -> IO ()
f >>* h = do
  (Result result) <- f
  B.resultNotifierSet result $ execResultM h

resultRawValue :: ResultM c a Value
resultRawValue = ResultM ask

result :: ValueGet a => ResultM c a a
result = resultRawValue >>= liftIO . valueGet

resultLength :: (ValueGet a, ValueGet [a]) => ResultM c [a] Integer
resultLength = resultRawValue >>= liftIO . listGetSize


class PersistentResultClass c

data Default

data Signal
instance PersistentResultClass Signal

data Broadcast
instance PersistentResultClass Broadcast

persist :: PersistentResultClass c => ResultM c a ()
persist = ResultM $ put True
