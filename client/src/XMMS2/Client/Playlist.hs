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
  ( -- * Types
    PlaylistChangedActions (..)
  , PlaylistChange (..)
  , PlaylistPosition

    -- * Commands
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
  , playlistCurrentActive
  , playlistInsertId
  , playlistRAdd
  , playlistRAddEncoded

    -- * Broadcasts
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


--------
-- Types

data PlaylistChange
  = PlaylistAdd
    { playlist    :: String
    , mlibId      :: MediaId
    , position    :: Int }
  | PlaylistInsert
    { playlist    :: String
    , mlibId      :: MediaId
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
    , mlibId      :: MediaId
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


-----------
-- Commands

playlistAddURL
  :: Connection
  -> Maybe String
  -> URL
  -> IO (Result Default ())
playlistAddURL xmmsc name url =
  liftResult $ B.playlistAddURL xmmsc name url

playlistAddId  ::
  Connection   ->
  Maybe String ->
  MediaId      ->
  IO (Result Default ())
playlistAddId xmmsc name id =
  liftResult $ B.playlistAddId xmmsc name id

playlistAddEncoded ::
  Connection       ->
  Maybe String     ->
  EncodedURL       ->
  IO (Result Default ())
playlistAddEncoded xmmsc name url =
  liftResult $ B.playlistAddEncoded xmmsc name url

playlistAddIdlist ::
  Connection      ->
  Maybe String    ->
  Coll            ->
  IO (Result Default ())
playlistAddIdlist xmmsc name idlist =
  liftResult $ B.playlistAddIdlist xmmsc name idlist

playlistAddCollection ::
  Connection          ->
  Maybe String        ->
  Coll                ->
  [String]            ->
  IO (Result Default ())
playlistAddCollection xmmsc name coll order =
  liftResult $ B.playlistAddCollection xmmsc name coll =<< newList order

playlistRemoveEntry ::
  Connection        ->
  Maybe String      ->
  Int               ->
  IO (Result Default ())
playlistRemoveEntry xmmsc name num =
  liftResult $ B.playlistRemoveEntry xmmsc name num

playlistClear  ::
  Connection   ->
  Maybe String ->
  IO (Result Default ())
playlistClear xmmsc name =
  liftResult $ B.playlistClear xmmsc name

playlistListEntries ::
  Connection        ->
  Maybe String      ->
  IO (Result Default [MediaId])
playlistListEntries xmmsc name =
  liftResult $ B.playlistListEntries xmmsc name

playlistSetNext ::
  Connection    ->
  Int32         ->
  IO (Result Default ())
playlistSetNext xmmsc num =
  liftResult $ B.playlistSetNext xmmsc num

playlistSetNextRel ::
  Connection       ->
  Int32            ->
  IO (Result Default ())
playlistSetNextRel xmmsc num =
  liftResult $ B.playlistSetNextRel xmmsc num

-- | Move a playlist entry to a new position. Both positions are
-- absolute.
playlistMoveEntry
  :: Connection
  -> Maybe String
  -> Int
  -> Int
  -> IO (Result Default ())
playlistMoveEntry xmmsc name from to =
  liftResult $ B.playlistMoveEntry xmmsc name from to

playlistCurrentPos ::
  Connection       ->
  Maybe String     ->
  IO (Result Default PlaylistPosition)
playlistCurrentPos xmmsc name =
  liftResult $ B.playlistCurrentPos xmmsc name

-- | Retrieve the name of the active playlist.
playlistCurrentActive ::
  Connection          ->
  IO (Result Default String)
playlistCurrentActive xmmsc =
  liftResult $ B.playlistCurrentActive xmmsc

playlistInsertId ::
  Connection     ->
  Maybe String   ->
  Int            ->
  MediaId        ->
  IO (Result Default ())
playlistInsertId xmmsc name pos id =
  liftResult $ B.playlistInsertId xmmsc name pos id

playlistRAdd   ::
  Connection   ->
  Maybe String ->
  URL          ->
  IO (Result Default ())
playlistRAdd xmmsc name url =
  liftResult $ B.playlistRAdd xmmsc name url

playlistRAddEncoded ::
  Connection        ->
  Maybe String      ->
  EncodedURL        ->
  IO (Result Default ())
playlistRAddEncoded xmmsc name url =
  liftResult $ B.playlistRAddEncoded xmmsc name url


-------------
-- Broadcasts

broadcastPlaylistChanged ::
  Connection             ->
  IO (Result Broadcast PlaylistChange)
broadcastPlaylistChanged =
  liftResult . B.broadcastPlaylistChanged

broadcastPlaylistCurrentPos ::
  Connection                ->
  IO (Result Broadcast ())
broadcastPlaylistCurrentPos =
  liftResult . B.broadcastPlaylistCurrentPos

broadcastPlaylistLoaded ::
  Connection            ->
  IO (Result Broadcast String)
broadcastPlaylistLoaded =
  liftResult . B.broadcastPlaylistLoaded
