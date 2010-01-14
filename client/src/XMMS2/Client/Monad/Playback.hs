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


instance ValueClass PlaybackStatus where
  valueGet v = liftM (toEnum . fromIntegral) $ getInt v


playbackStop = liftXMMSResult XP.playbackStop

playbackTickle = liftXMMSResult XP.playbackTickle

playbackStart = liftXMMSResult XP.playbackStart
                 
playbackPause = liftXMMSResult XP.playbackPause

playbackCurrentId = liftXMMSResult XP.playbackCurrentId

playbackSeekMs pos =
  liftXMMSResult $ \xmmsc -> XP.playbackSeekMs xmmsc pos

playbackSeekMsRel pos =
  liftXMMSResult $ \xmmsc -> XP.playbackSeekMsRel xmmsc pos

playbackSeekSamples pos =
  liftXMMSResult $ \xmmsc -> XP.playbackSeekSamples xmmsc pos

playbackSeekSamplesRel pos =
  liftXMMSResult $ \xmmsc -> XP.playbackSeekSamplesRel xmmsc pos

playbackPlaytime = liftXMMSResult XP.playbackPlaytime

playbackStatus = liftXMMSResult XP.playbackStatus

playbackVolumeSet channel volume =
  liftXMMSResult $ \xmmsc -> XP.playbackVolumeSet xmmsc channel volume

playbackVolumeGet = liftXMMSResult XP.playbackVolumeGet


broadcastPlaybackVolumeChanged =
  liftXMMSResult XP.broadcastPlaybackVolumeChanged

broadcastPlaybackStatus = liftXMMSResult XP.broadcastPlaybackStatus

broadcastPlaybackCurrentId = liftXMMSResult XP.broadcastPlaybackCurrentId
                         

signalPlaybackPlaytime = liftXMMSResult XP.signalPlaybackPlaytime
