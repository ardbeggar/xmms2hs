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
  ( XMMS
  , runXMMS
  , connection
  , liftIO
  , liftXMMS
  , module Control.Monad.Exception
  ) where

import Control.Monad.Reader
import XMMS2.Client.Connection (Connection)
import Control.Monad.Exception
  

type XMMS = ReaderT Connection IO

runXMMS :: XMMS a -> Connection -> IO a
runXMMS = runReaderT

connection :: XMMS Connection
connection = ask

liftXMMS f = do
  xmmsc <- connection
  liftIO $ f xmmsc
         