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
  , PlaylistPosition
  , playlistCurrentPos
  , broadcastPlaylistChanged
  , broadcastPlaylistCurrentPos
  ) where

import Control.Monad
import Control.Monad.Error  
import Data.Maybe  
import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Value
import XMMS2.Client.Monad.Result
import qualified XMMS2.Client.Playlist as XP
import qualified Data.Map as Map  


playlistListEntries :: Maybe String -> XMMS (Result [Int32])
playlistListEntries name =
  liftXMMSResult $ \xmmsc -> XP.playlistListEntries xmmsc name

playlistSetNext :: Int32 -> XMMS (Result ())
playlistSetNext n =
  liftXMMSResult $ \xmmsc -> XP.playlistSetNext xmmsc n


type PlaylistPosition = (Int32, String)

instance ValueTypeClass PlaylistPosition where
  valueToType v = do
    dict <- getDict v
    case (Map.lookup "position" dict,
          Map.lookup "name"     dict) of
      (Just (DataInt32 p), Just (DataString n)) ->
        return (p, n)
      _ ->
        throwError "not a playlist position"

playlistCurrentPos :: Maybe String -> XMMS (Result PlaylistPosition)
playlistCurrentPos name =
  liftXMMSResult $ \xmmsc -> XP.playlistCurrentPos xmmsc name

                             
broadcastPlaylistChanged :: XMMS (Result ())
broadcastPlaylistChanged = liftXMMSResult XP.broadcastPlaylistChanged

broadcastPlaylistCurrentPos :: XMMS (Result ())
broadcastPlaylistCurrentPos = liftXMMSResult XP.broadcastPlaylistCurrentPos
              