-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 14 Jan. 2010
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

module Control.Monad.ToIO
  ( MonadToIO (..)
  ) where

import Control.Monad.Reader


class Monad m => MonadToIO m where
  toIO :: m (m a -> IO a)
          

instance MonadToIO IO where
  toIO = return id

instance MonadToIO m => MonadToIO (ReaderT r m) where
  toIO = do
    r <- ask
    w <- lift toIO
    return $ w . flip runReaderT r
