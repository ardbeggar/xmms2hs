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
import Control.Exception  
import XMMS2.Client.Exception
import XMMS2.Client.Connection (Connection)
import Prelude hiding (catch)  


class MonadException m where
  throwM :: Exception e => e -> m a
  catchM :: Exception e => m a -> (e -> m a) -> m a

tryM a = catchM (Right `liftM` a) (return . Left)

instance MonadException IO where
  throwM = throwIO
  catchM = catch

instance MonadException (ReaderT r IO) where
  throwM e = liftIO $ throwIO e
  catchM f h = ReaderT (\r -> do
                          v <- try $ runReaderT f r
                          case v of
                            Right v' -> return v'
                            Left exc -> runReaderT (h exc) r)

type XMMS = ReaderT Connection IO

runXMMS :: XMMS a -> Connection -> IO a
runXMMS = runReaderT


connection :: XMMS Connection
connection = ask


liftXMMS f = do
  xmmsc <- connection
  liftIO $ f xmmsc
         