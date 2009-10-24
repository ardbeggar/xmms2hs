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


instance ValueClass a PlaybackStatus where
  valueGet v = liftM (toEnum . fromIntegral) $ getInt v


playbackStop :: MonadXMMS m => m (Result ())
playbackStop = liftXMMSResult XP.playbackStop

playbackTickle :: MonadXMMS m => m (Result ())               
playbackTickle = liftXMMSResult XP.playbackTickle

playbackStart :: MonadXMMS m => m (Result ())
playbackStart = liftXMMSResult XP.playbackStart
                 
playbackPause :: MonadXMMS m => m (Result ())
playbackPause = liftXMMSResult XP.playbackPause

playbackCurrentId :: MonadXMMS m => m (Result Int32)
playbackCurrentId = liftXMMSResult XP.playbackCurrentId

playbackSeekMs :: MonadXMMS m => Int32 -> m (Result ())
playbackSeekMs pos =
  liftXMMSResult $ \xmmsc -> XP.playbackSeekMs xmmsc pos

playbackSeekMsRel :: MonadXMMS m => Int32 -> m (Result ())
playbackSeekMsRel pos =
  liftXMMSResult $ \xmmsc -> XP.playbackSeekMsRel xmmsc pos

playbackSeekSamples :: MonadXMMS m => Int32 -> m (Result ())
playbackSeekSamples pos =
  liftXMMSResult $ \xmmsc -> XP.playbackSeekSamples xmmsc pos

playbackSeekSamplesRel :: MonadXMMS m => Int32 -> m (Result ())
playbackSeekSamplesRel pos =
  liftXMMSResult $ \xmmsc -> XP.playbackSeekSamplesRel xmmsc pos

playbackPlaytime :: MonadXMMS m => m (Result Int32)
playbackPlaytime = liftXMMSResult XP.playbackPlaytime

playbackStatus :: MonadXMMS m => m (Result PlaybackStatus)
playbackStatus = liftXMMSResult XP.playbackStatus

playbackVolumeSet :: MonadXMMS m => String -> Int -> m (Result ())
playbackVolumeSet channel volume =
  liftXMMSResult $ \xmmsc -> XP.playbackVolumeSet xmmsc channel volume

playbackVolumeGet :: MonadXMMS m => m (Result (Dict Int32))
playbackVolumeGet = liftXMMSResult XP.playbackVolumeGet


broadcastPlaybackVolumeChanged :: MonadXMMS m => m (Result (Dict Int32))
broadcastPlaybackVolumeChanged =
  liftXMMSResult XP.broadcastPlaybackVolumeChanged

broadcastPlaybackStatus :: MonadXMMS m => m (Result PlaybackStatus)
broadcastPlaybackStatus = liftXMMSResult XP.broadcastPlaybackStatus

broadcastPlaybackCurrentId :: MonadXMMS m => m (Result Int32)
broadcastPlaybackCurrentId = liftXMMSResult XP.broadcastPlaybackCurrentId
                         

signalPlaybackPlaytime :: MonadXMMS m => m (Result Int32)
signalPlaybackPlaytime = liftXMMSResult XP.signalPlaybackPlaytime
