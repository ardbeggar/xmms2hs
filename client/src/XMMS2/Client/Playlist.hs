-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 15 Feb. 2010
--
--  Copyright (C) 2009-2010 Oleg Belozeorov
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

module XMMS2.Client.Playlist
  ( PlaylistPosition
  , playlistAddURL
  , playlistAddId
  , playlistAddEncoded
  , playlistAddIdlist
  , playlistAddCollection
  , playlistRemoveEntry
  , playlistClear
  , playlistListEntries
  , playlistSetNext
  , playlistSetNextRel
  , playlistMoveEntry
  , playlistCurrentPos
  , playlistInsertId
  , playlistRAdd
  , playlistRAddEncoded
  , broadcastPlaylistChanged
  , broadcastPlaylistCurrentPos
  , broadcastPlaylistLoaded
  ) where

import Control.Applicative

import XMMS2.Client.Types
import XMMS2.Client.Result

import XMMS2.Client.Bindings.Connection
import XMMS2.Client.Bindings.Playlist (PlaylistPosition (..))
import qualified XMMS2.Client.Bindings.Playlist as B


instance ValueGet PlaylistPosition where
  valueGet v = do
    dict <- valueGet v
    maybe (fail "not a playlist position") return $
      (,) <$> lookupInt32 "position" dict <*> lookupString "name" dict


playlistAddURL :: Connection -> Maybe String -> String -> IO (Result ())
playlistAddURL xmmsc pls url =
  liftResult $ B.playlistAddURL xmmsc pls url

playlistAddId :: Connection -> Maybe String -> Int32 -> IO (Result ())
playlistAddId xmmsc pls id =
  liftResult $ B.playlistAddId xmmsc pls id

playlistAddEncoded :: Connection -> Maybe String -> String -> IO (Result ())
playlistAddEncoded xmmsc pls url =
  liftResult $ B.playlistAddEncoded xmmsc pls url

playlistAddIdlist :: Connection -> Maybe String -> Coll -> IO (Result ())
playlistAddIdlist xmmsc pls idlist =
  liftResult $ B.playlistAddIdlist xmmsc pls idlist

playlistAddCollection
  :: Connection
  -> Maybe String
  -> Coll
  -> [String]
  -> IO (Result ())
playlistAddCollection xmmsc pls coll order = do
  liftResult $ B.playlistAddCollection xmmsc pls coll =<< newList order

playlistRemoveEntry :: Connection -> Maybe String -> Int -> IO (Result ())
playlistRemoveEntry xmmsc pls num =
  liftResult $ B.playlistRemoveEntry xmmsc pls num

playlistClear :: Connection -> Maybe String -> IO (Result ())
playlistClear xmmsc pls =
  liftResult $ B.playlistClear xmmsc pls

playlistListEntries :: Connection -> Maybe String -> IO (Result [Int32])
playlistListEntries xmmsc pls =
  liftResult $ B.playlistListEntries xmmsc pls

playlistSetNext :: Connection -> Int32 -> IO (Result ())
playlistSetNext xmmsc num =
  liftResult $ B.playlistSetNext xmmsc num

playlistSetNextRel :: Connection -> Int32 -> IO (Result ())
playlistSetNextRel xmmsc num =
  liftResult $ B.playlistSetNextRel xmmsc num

playlistMoveEntry :: Connection -> Maybe String -> Int -> Int -> IO (Result ())
playlistMoveEntry xmmsc pls from to =
  liftResult $ B.playlistMoveEntry xmmsc pls from to

playlistCurrentPos :: Connection -> Maybe String -> IO (Result PlaylistPosition)
playlistCurrentPos xmmsc pls =
  liftResult $ B.playlistCurrentPos xmmsc pls

playlistInsertId :: Connection -> Maybe String -> Int -> Int32 -> IO (Result ())
playlistInsertId xmmsc pls pos id =
  liftResult $ B.playlistInsertId xmmsc pls pos id

playlistRAdd :: Connection -> Maybe String -> String -> IO (Result ())
playlistRAdd xmmsc pls url =
  liftResult $ B.playlistRAdd xmmsc pls url

playlistRAddEncoded :: Connection -> Maybe String -> String -> IO (Result ())
playlistRAddEncoded xmmsc pls url =
  liftResult $ B.playlistRAddEncoded xmmsc pls url


broadcastPlaylistChanged :: Connection -> IO (Result ())
broadcastPlaylistChanged =
  liftResult . B.broadcastPlaylistChanged

broadcastPlaylistCurrentPos :: Connection -> IO (Result ())
broadcastPlaylistCurrentPos =
  liftResult . B.broadcastPlaylistCurrentPos

broadcastPlaylistLoaded :: Connection -> IO (Result String)
broadcastPlaylistLoaded =
  liftResult . B.broadcastPlaylistLoaded
