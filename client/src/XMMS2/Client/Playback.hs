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

module XMMS2.Client.Playback
  ( PlaybackStatus (..)
  , SeekMode (..)
  , playbackStop
  , playbackTickle
  , playbackStart
  , playbackPause
  , playbackCurrentId
  , playbackSeekMs
  , playbackSeekSamples
  , playbackPlaytime
  , playbackStatus
  , playbackVolumeSet
  , playbackVolumeGet
  , broadcastPlaybackVolumeChanged
  , broadcastPlaybackStatus
  , broadcastPlaybackCurrentId
  , signalPlaybackPlaytime
  ) where

import Control.Applicative

import XMMS2.Client.Types
import XMMS2.Client.Result

import XMMS2.Client.Bindings.Connection
import XMMS2.Client.Bindings.Playback (PlaybackStatus (..), SeekMode (..))
import qualified XMMS2.Client.Bindings.Playback as B


instance ValueGet PlaybackStatus where
  valueGet v = (toEnum . fromIntegral) <$> getInt v


playbackStop :: Connection -> IO (Result Default ())
playbackStop xmmsc =
  liftResult $ B.playbackStop xmmsc

playbackTickle :: Connection -> IO (Result Default ())
playbackTickle xmmsc =
  liftResult $ B.playbackTickle xmmsc

playbackStart :: Connection -> IO (Result Default ())
playbackStart xmmsc =
  liftResult $ B.playbackStart xmmsc

playbackPause :: Connection -> IO (Result Default ())
playbackPause xmmsc =
  liftResult $ B.playbackPause xmmsc

playbackCurrentId :: Connection -> IO (Result Default Int32)
playbackCurrentId xmmsc =
  liftResult $ B.playbackCurrentId xmmsc

playbackSeekMs :: Connection -> Int32 -> SeekMode -> IO (Result Default ())
playbackSeekMs xmmsc pos whence
  = liftResult $ B.playbackSeekMs xmmsc pos whence

playbackSeekSamples :: Connection -> Int32 -> SeekMode -> IO (Result Default ())
playbackSeekSamples xmmsc pos whence =
  liftResult $ B.playbackSeekSamples xmmsc pos whence

playbackPlaytime :: Connection -> IO (Result Default Int32)
playbackPlaytime xmmsc =
  liftResult $ B.playbackPlaytime xmmsc

playbackStatus :: Connection -> IO (Result Default PlaybackStatus)
playbackStatus xmmsc =
  liftResult $ B.playbackStatus xmmsc

playbackVolumeSet :: Connection -> String -> Int -> IO (Result Default ())
playbackVolumeSet xmmsc chan vol =
  liftResult $ B.playbackVolumeSet xmmsc chan vol

playbackVolumeGet :: Connection -> IO (Result Default (Dict Int32))
playbackVolumeGet xmmsc =
  liftResult $ B.playbackVolumeGet xmmsc


broadcastPlaybackVolumeChanged :: Connection -> IO (Result Broadcast (Dict Int32))
broadcastPlaybackVolumeChanged =
  liftResult . B.broadcastPlaybackVolumeChanged

broadcastPlaybackStatus :: Connection -> IO (Result Broadcast PlaybackStatus)
broadcastPlaybackStatus =
  liftResult . B.broadcastPlaybackStatus

broadcastPlaybackCurrentId :: Connection -> IO (Result Broadcast Int32)
broadcastPlaybackCurrentId =
  liftResult . B.broadcastPlaybackCurrentId


signalPlaybackPlaytime :: Connection -> IO (Result Signal Int32)
signalPlaybackPlaytime =
  liftResult . B.signalPlaybackPlaytime
