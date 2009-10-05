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
  ( MonadException (..)
  , XMMS
  , runXMMS
  , connection
  , liftIO
  , liftXMMS
  , tryM
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Exception  
import XMMS2.Client.Exception
import XMMS2.Client.Connection (Connection)
import Prelude hiding (catch)  


class MonadIO m => MonadException m where
  throwM :: Exception e => e -> m a
  throwM e = liftIO $ throwIO e
  catchM :: Exception e => m a -> (e -> m a) -> m a

tryM a = catchM (Right `liftM` a) (return . Left)

instance MonadException IO where
  catchM = catch

instance MonadException m => MonadException (ReaderT r m) where
  catchM f h = ReaderT (\r -> do
                          v <- tryM $ runReaderT f r
                          case v of
                            Right v' -> return v'
                            Left exc -> runReaderT (h exc) r)

instance MonadException m => MonadException (StateT s m) where
  catchM f h = StateT (\s -> do
                          v <- tryM $ runStateT f s
                          case v of
                            Right v' -> return v'
                            Left exc -> runStateT (h exc) s)

type XMMS = ReaderT Connection IO

runXMMS :: XMMS a -> Connection -> IO a
runXMMS = runReaderT


connection :: XMMS Connection
connection = ask


liftXMMS f = do
  xmmsc <- connection
  liftIO $ f xmmsc
         