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
  ( XMMSM
  , XMMSCM (..)
  , liftIO
  , ToIO (..)
  , XMMS
  , runXMMS
  , module Control.Monad.Exception
  ) where

import Control.Monad.Reader
import XMMS2.Client.Connection (Connection)
import Control.Monad.Exception

class Monad m => ToIO m where
  toIO :: m (m a -> IO a)

instance ToIO IO where
  toIO = return id

instance ToIO m => ToIO (ReaderT r m) where
  toIO = do
    r <- ask
    w <- lift toIO
    return $ w . flip runReaderT r

class (MonadIO m, MonadException m, ToIO m) => XMMSM m

class XMMSM m => XMMSCM e m | m -> e where
  connection  :: m Connection
  environment :: m e
  liftXMMS    :: (Connection -> IO a) -> m a
  liftXMMS f = connection >>= liftIO . f

instance XMMSM IO  

type XMMS e m = ReaderT (Connection, e) m

runXMMS f c e = runReaderT f (c, e)

instance XMMSM m => XMMSM (ReaderT (Connection, e) m)
  
instance XMMSM m => XMMSCM e (ReaderT (Connection, e) m) where
  connection  = asks fst
  environment = asks snd

