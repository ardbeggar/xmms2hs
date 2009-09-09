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
  ( playlistListEntries
  , playlistSetNext
  , broadcastPlaylistChanged
  ) where

import Control.Monad
import Data.Maybe  
import XMMS2.Client.Monad.Monad
import Data.Int (Int32)  
import XMMS2.Client.Monad.Result
import qualified XMMS2.Client.Playlist as XP


playlistListEntries :: Maybe String -> XMMS (Result [Int32])
playlistListEntries name =
  liftXMMSResult $ \xmmsc -> XP.playlistListEntries xmmsc name

playlistSetNext :: Int32 -> XMMS (Result ())
playlistSetNext n =
  liftXMMSResult $ \xmmsc -> XP.playlistSetNext xmmsc n

                             
broadcastPlaylistChanged :: XMMS (Result ())
broadcastPlaylistChanged = liftXMMSResult XP.broadcastPlaylistChanged
              