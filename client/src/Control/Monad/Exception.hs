-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Oct. 2009
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

module Control.Monad.Exception
  ( MonadException (..)
  , tryM
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Exception  
import Prelude hiding (catch)
  

class MonadIO m => MonadException m where
  throwM :: Exception e => e -> m a
  throwM = liftIO . throwIO
  catchM :: Exception e => m a -> (e -> m a) -> m a

tryM a = catchM (Right `liftM` a) (return . Left)
         

instance MonadException IO where
  throwM = throwIO
  catchM = catch

instance MonadException m => MonadException (ReaderT r m) where
  catchM f h = ReaderT $ \r -> runReaderT f r `catchM` \e -> runReaderT (h e) r

instance MonadException m => MonadException (StateT s m) where
  catchM f h = StateT $ \s -> runStateT f s `catchM` \e -> runStateT (h e) s
