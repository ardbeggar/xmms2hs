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


playlistAddURL :: MonadXMMS m => Maybe String -> String -> m (Result ())
playlistAddURL name url =
  liftXMMSResult $ \xmmsc -> XP.playlistAddURL xmmsc name url

playlistAddId :: MonadXMMS m => Maybe String -> Int32 -> m (Result ())
playlistAddId name id =
  liftXMMSResult $ \xmmsc -> XP.playlistAddId xmmsc name id

playlistAddEncoded :: MonadXMMS m => Maybe String -> String -> m (Result ())
playlistAddEncoded name url =
  liftXMMSResult $ \xmmsc -> XP.playlistAddEncoded xmmsc name url

playlistAddIdlist :: MonadXMMS m => Maybe String -> Coll -> m (Result ())
playlistAddIdlist name coll =
  liftXMMSResult $ \xmmsc -> XP.playlistAddIdlist xmmsc name coll

playlistRemoveEntry :: MonadXMMS m => Maybe String -> Int -> m (Result ())
playlistRemoveEntry name pos =
  liftXMMSResult $ \xmmsc -> XP.playlistRemoveEntry xmmsc name pos
                             
playlistClear :: MonadXMMS m => Maybe String -> m (Result ())
playlistClear name =
  liftXMMSResult $ \xmmsc -> XP.playlistClear xmmsc name
                             
playlistListEntries :: MonadXMMS m => Maybe String -> m (Result [Int32])
playlistListEntries name =
  liftXMMSResult $ \xmmsc -> XP.playlistListEntries xmmsc name

playlistSetNext :: MonadXMMS m => Int32 -> m (Result ())
playlistSetNext n =
  liftXMMSResult $ \xmmsc -> XP.playlistSetNext xmmsc n

playlistSetNextRel :: MonadXMMS m => Int32 -> m (Result ())
playlistSetNextRel n =
  liftXMMSResult $ \xmmsc -> XP.playlistSetNextRel xmmsc n

playlistMoveEntry :: MonadXMMS m => Maybe String -> Int -> Int -> m (Result ())
playlistMoveEntry name from to =
  liftXMMSResult $ \xmmsc -> XP.playlistMoveEntry xmmsc name from to


playlistCurrentPos :: MonadXMMS m => Maybe String -> m (Result PlaylistPosition)
playlistCurrentPos name =
  liftXMMSResult $ \xmmsc -> XP.playlistCurrentPos xmmsc name

playlistInsertId :: MonadXMMS m => Maybe String -> Int -> Int32 -> m (Result ())
playlistInsertId name pos id =
  liftXMMSResult $ \xmmsc -> XP.playlistInsertId xmmsc name pos id

playlistRAdd :: MonadXMMS m => Maybe String -> String -> m (Result ())
playlistRAdd name url =
  liftXMMSResult $ \xmmsc -> XP.playlistRAdd xmmsc name url

playlistRAddEncoded :: MonadXMMS m => Maybe String -> String -> m (Result ())
playlistRAddEncoded name url =
  liftXMMSResult $ \xmmsc -> XP.playlistRAddEncoded xmmsc name url

                             
broadcastPlaylistChanged :: MonadXMMS m => m (Result ())
broadcastPlaylistChanged = liftXMMSResult XP.broadcastPlaylistChanged

broadcastPlaylistCurrentPos :: MonadXMMS m => m (Result ())
broadcastPlaylistCurrentPos = liftXMMSResult XP.broadcastPlaylistCurrentPos

broadcastPlaylistLoaded :: MonadXMMS m => m (Result String)
broadcastPlaylistLoaded = liftXMMSResult XP.broadcastPlaylistLoaded
              