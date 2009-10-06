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

module XMMS2.Client.Monad.Monad
  ( MonadXMMS (..)
  , liftXMMS
  , liftIO
  , XMMS
  , module Control.Monad.Exception
  ) where

import Control.Monad.Reader
import XMMS2.Client.Connection (Connection)
import Control.Monad.Exception


class (MonadIO m, MonadException m) => MonadXMMS m where
  connection :: m Connection
  runXMMS :: m a -> Connection -> IO a

liftXMMS :: MonadXMMS m => (Connection -> IO a) -> m a
liftXMMS f = do
  xmmsc <- connection
  liftIO $ f xmmsc
  

type XMMS = ReaderT Connection IO

instance MonadXMMS (ReaderT Connection IO) where
  connection = ask
  runXMMS = runReaderT
