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
  ( PlaylistChangedActions (..)
  , PlaylistChange (..)
  , PlaylistPosition
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
import XMMS2.Client.Bindings.Playlist (PlaylistChangedActions (..))
import qualified XMMS2.Client.Bindings.Playlist as B


data PlaylistChange
  = PlaylistAdd
    { playlist    :: String
    , mlibId      :: Int32
    , position    :: Int }
  | PlaylistInsert
    { playlist    :: String
    , mlibId      :: Int32
    , position    :: Int }
  | PlaylistShuffle
    { playlist    :: String }
  | PlaylistRemove
    { playlist    :: String
    , position    :: Int }
  | PlaylistClear
    { playlist    :: String }
  | PlaylistMove
    { playlist    :: String
    , mlibId      :: Int32
    , position    :: Int
    , newPosition :: Int }
  | PlaylistSort
    { playlist    :: String }
  | PlaylistUpdate
    { playlist    :: String }
  deriving (Show)

instance ValueGet PlaylistChange where
  valueGet v = do
    dict <- valueGet v
    let getMlibId      = lookupInt32 "id" dict
        getPosition    = fromIntegral <$> lookupInt32 "position" dict
        getNewPosition = fromIntegral <$> lookupInt32 "newposition" dict
    maybe (fail "not a playlist change notification") return $ do
      playlist <- lookupString "name" dict
      action   <- (toEnum . fromIntegral) <$> lookupInt32 "type" dict
      case action of
        PlaylistChangedAdd -> do
          mlibId   <- getMlibId
          position <- getPosition
          return PlaylistAdd { playlist = playlist
                             , mlibId   = mlibId
                             , position = position }
        PlaylistChangedInsert -> do
          mlibId   <- getMlibId
          position <- getPosition
          return PlaylistInsert { playlist = playlist
                                , mlibId   = mlibId
                                , position = position }
        PlaylistChangedShuffle ->
          return PlaylistShuffle { playlist = playlist }
        PlaylistChangedRemove -> do
          position <- getPosition
          return PlaylistRemove { playlist = playlist
                                , position = position }
        PlaylistChangedClear ->
          return PlaylistClear { playlist = playlist }
        PlaylistChangedMove -> do
          mlibId      <- getMlibId
          position    <- getPosition
          newPosition <- getNewPosition
          return PlaylistMove { playlist    = playlist
                              , mlibId      = mlibId
                              , position    = position
                              , newPosition = newPosition }
        PlaylistChangedSort ->
          return PlaylistSort { playlist = playlist }
        PlaylistChangedUpdate ->
          return PlaylistUpdate { playlist = playlist }


type PlaylistPosition = (Int32, String)

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


broadcastPlaylistChanged :: Connection -> IO (Result PlaylistChange)
broadcastPlaylistChanged =
  liftResult . B.broadcastPlaylistChanged

broadcastPlaylistCurrentPos :: Connection -> IO (Result ())
broadcastPlaylistCurrentPos =
  liftResult . B.broadcastPlaylistCurrentPos

broadcastPlaylistLoaded :: Connection -> IO (Result String)
broadcastPlaylistLoaded =
  liftResult . B.broadcastPlaylistLoaded
