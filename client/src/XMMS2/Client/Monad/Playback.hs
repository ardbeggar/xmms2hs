-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 8 Sep. 2009
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

module XMMS2.Client.Monad.Playback
  ( PlaybackStatus (..)
  , playbackStop
  , playbackTickle
  , playbackStart
  , playbackPause
  , playbackCurrentId
  , playbackSeekMs
  , playbackSeekMsRel
  , playbackSeekSamples
  , playbackSeekSamplesRel
  , playbackPlaytime
  , playbackStatus
  , playbackVolumeSet
  , playbackVolumeGet
  , broadcastPlaybackVolumeChanged
  , broadcastPlaybackStatus
  , broadcastPlaybackCurrentId
  , signalPlaybackPlaytime
  ) where

import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Value
import XMMS2.Client.Monad.Result
import XMMS2.Client.Playback (PlaybackStatus)
import qualified XMMS2.Client.Playback as XP
import Control.Monad


instance ValueGet PlaybackStatus where
  valueGet v = liftM (toEnum . fromIntegral) $ getInt v


playbackStop = liftXMMS XP.playbackStop

playbackTickle = liftXMMS XP.playbackTickle

playbackStart = liftXMMS XP.playbackStart

playbackPause = liftXMMS XP.playbackPause

playbackCurrentId = liftXMMS XP.playbackCurrentId

playbackSeekMs pos =
  liftXMMS $ \xmmsc -> XP.playbackSeekMs xmmsc pos

playbackSeekMsRel pos =
  liftXMMS $ \xmmsc -> XP.playbackSeekMsRel xmmsc pos

playbackSeekSamples pos =
  liftXMMS $ \xmmsc -> XP.playbackSeekSamples xmmsc pos

playbackSeekSamplesRel pos =
  liftXMMS $ \xmmsc -> XP.playbackSeekSamplesRel xmmsc pos

playbackPlaytime = liftXMMS XP.playbackPlaytime

playbackStatus = liftXMMS XP.playbackStatus

playbackVolumeSet channel volume =
  liftXMMS $ \xmmsc -> XP.playbackVolumeSet xmmsc channel volume

playbackVolumeGet = liftXMMS XP.playbackVolumeGet


broadcastPlaybackVolumeChanged =
  liftXMMS XP.broadcastPlaybackVolumeChanged

broadcastPlaybackStatus = liftXMMS XP.broadcastPlaybackStatus

broadcastPlaybackCurrentId = liftXMMS XP.broadcastPlaybackCurrentId


signalPlaybackPlaytime = liftXMMS XP.signalPlaybackPlaytime
