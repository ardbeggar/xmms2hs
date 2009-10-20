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
  , broadcastPlaylistChanged
  , broadcastPlaylistCurrentPos
  ) where

import Control.Monad
import Data.Maybe
import Control.Exception  
import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Value
import XMMS2.Client.Monad.Result
import XMMS2.Client.Monad.Coll  
import qualified XMMS2.Client.Playlist as XP
import qualified Data.Map as Map  


playlistAddURL :: MonadXMMS m => Maybe String -> String -> m (Result ())
playlistAddURL name url =
  liftXMMSResult $ \xmmsc -> XP.playlistAddURL xmmsc name url

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


type PlaylistPosition = (Int32, String)

instance ValueClass PlaylistPosition where
  valueGet v = do
    dict <- getDict v
    case (Map.lookup "position" dict,
          Map.lookup "name"     dict) of
      (Just (DataInt32 p), Just (DataString n)) ->
        return (p, n)
      _ ->
        throwM $ AssertionFailed "playlist position"

playlistCurrentPos :: MonadXMMS m => Maybe String -> m (Result PlaylistPosition)
playlistCurrentPos name =
  liftXMMSResult $ \xmmsc -> XP.playlistCurrentPos xmmsc name

playlistInsertId :: MonadXMMS m => Maybe String -> Int -> Int32 -> m (Result ())
playlistInsertId name pos id =
  liftXMMSResult $ \xmmsc -> XP.playlistInsertId xmmsc name pos id

                             
broadcastPlaylistChanged :: MonadXMMS m => m (Result ())
broadcastPlaylistChanged = liftXMMSResult XP.broadcastPlaylistChanged

broadcastPlaylistCurrentPos :: MonadXMMS m => m (Result ())
broadcastPlaylistCurrentPos = liftXMMSResult XP.broadcastPlaylistCurrentPos
              