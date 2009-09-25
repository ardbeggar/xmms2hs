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
  , playbackPlaytime
  , playbackStatus
  , broadcastPlaybackCurrentId
  , broadcastPlaybackStatus
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


playbackStop :: XMMS (Result ())
playbackStop = liftXMMSResult XP.playbackStop

playbackTickle :: XMMS (Result ())               
playbackTickle = liftXMMSResult XP.playbackTickle

playbackStart :: XMMS (Result ())
playbackStart = liftXMMSResult XP.playbackStart
                 
playbackPause :: XMMS (Result ())
playbackPause = liftXMMSResult XP.playbackPause

playbackCurrentId :: XMMS (Result Int32)
playbackCurrentId = liftXMMSResult XP.playbackCurrentId

playbackSeekMs :: Int32 -> XMMS (Result ())
playbackSeekMs pos =
  liftXMMSResult $ \xmmsc -> XP.playbackSeekMs xmmsc pos

playbackPlaytime :: XMMS (Result Int32)
playbackPlaytime = liftXMMSResult XP.playbackPlaytime

playbackStatus :: XMMS (Result PlaybackStatus)
playbackStatus = liftXMMSResult XP.playbackStatus


broadcastPlaybackStatus :: XMMS (Result PlaybackStatus)
broadcastPlaybackStatus = liftXMMSResult XP.broadcastPlaybackStatus

broadcastPlaybackCurrentId :: XMMS (Result Int32)
broadcastPlaybackCurrentId = liftXMMSResult XP.broadcastPlaybackCurrentId
                         

signalPlaybackPlaytime :: XMMS (Result Int32)
signalPlaybackPlaytime = liftXMMSResult XP.signalPlaybackPlaytime
