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

module XMMS2.Client.Monad.Playlist
  ( listEntries
  , setNext
  , broadcastPlaylistChanged
  ) where

import Control.Monad
import Data.Maybe  
import XMMS2.Client.Monad.Monad
import Data.Int (Int32)  
import XMMS2.Client.Monad.Result
import qualified XMMS2.Client.Playlist as XP


listEntries :: Maybe String -> XMMS (Result [Int32])
listEntries name = do
  r <- liftXMMS $ \xmmsc -> XP.listEntries xmmsc name
  return $ Result r

setNext n = liftXMMS $ \xmmsc -> XP.setNext xmmsc n
         
broadcastPlaylistChanged :: XMMS (Result ())
broadcastPlaylistChanged = do
  r <- liftXMMS $ XP.broadcastPlaylistChanged
  return $ Result r
              