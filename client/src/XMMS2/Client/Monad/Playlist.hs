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
  ( playlistAddURL
  , playlistAddId
  , playlistAddEncoded
  , playlistAddIdlist
  , playlistRemoveEntry
  , playlistClear
  , playlistListEntries
  , playlistSetNext
  , playlistSetNextRel
  , playlistMoveEntry
  , PlaylistPosition
  , playlistCurrentPos
  , playlistInsertId
  , playlistRAdd
  , playlistRAddEncoded
  , broadcastPlaylistChanged
  , broadcastPlaylistCurrentPos
  , broadcastPlaylistLoaded
  ) where

import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Control.Exception  
import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Value
import XMMS2.Client.Monad.Result
import XMMS2.Client.Monad.Coll  
import XMMS2.Client.Playlist (PlaylistPosition)
import qualified XMMS2.Client.Playlist as XP
import qualified Data.Map as Map  


playlistAddURL name url =
  liftXMMS $ \xmmsc -> XP.playlistAddURL xmmsc name url

playlistAddId name id =
  liftXMMS $ \xmmsc -> XP.playlistAddId xmmsc name id

playlistAddEncoded name url =
  liftXMMS $ \xmmsc -> XP.playlistAddEncoded xmmsc name url

playlistAddIdlist name coll =
  liftXMMS $ \xmmsc -> XP.playlistAddIdlist xmmsc name coll

playlistRemoveEntry name pos =
  liftXMMS $ \xmmsc -> XP.playlistRemoveEntry xmmsc name pos
                             
playlistClear name =
  liftXMMS $ \xmmsc -> XP.playlistClear xmmsc name
                             
playlistListEntries name =
  liftXMMS $ \xmmsc -> XP.playlistListEntries xmmsc name

playlistSetNext n =
  liftXMMS $ \xmmsc -> XP.playlistSetNext xmmsc n

playlistSetNextRel n =
  liftXMMS $ \xmmsc -> XP.playlistSetNextRel xmmsc n

playlistMoveEntry name from to =
  liftXMMS $ \xmmsc -> XP.playlistMoveEntry xmmsc name from to


playlistCurrentPos name =
  liftXMMS $ \xmmsc -> XP.playlistCurrentPos xmmsc name

playlistInsertId name pos id =
  liftXMMS $ \xmmsc -> XP.playlistInsertId xmmsc name pos id

playlistRAdd name url =
  liftXMMS $ \xmmsc -> XP.playlistRAdd xmmsc name url

playlistRAddEncoded name url =
  liftXMMS $ \xmmsc -> XP.playlistRAddEncoded xmmsc name url

                             
broadcastPlaylistChanged = liftXMMS XP.broadcastPlaylistChanged

broadcastPlaylistCurrentPos = liftXMMS XP.broadcastPlaylistCurrentPos

broadcastPlaylistLoaded = liftXMMS XP.broadcastPlaylistLoaded
              