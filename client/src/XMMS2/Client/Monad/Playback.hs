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
  , playbackStart
  , playbackStop
  , playbackTickle
  , playbackStatus
  , broadcastPlaybackStatus
  ) where

import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Value
import XMMS2.Client.Monad.Result
import XMMS2.Client.Playback (PlaybackStatus)  
import qualified XMMS2.Client.Playback as XP
import Control.Monad


instance ResultType PlaybackStatus where
  valueToType v = liftM (toEnum . fromIntegral) $ getInt v


playbackStart :: XMMS (Result ())
playbackStart = liftXMMSResult XP.playbackStart
                 
playbackStop :: XMMS (Result ())
playbackStop = liftXMMSResult XP.playbackStop

playbackTickle :: XMMS (Result ())               
playbackTickle = liftXMMSResult XP.playbackTickle

playbackStatus :: XMMS (Result PlaybackStatus)
playbackStatus = liftXMMSResult XP.playbackStatus

  
broadcastPlaybackStatus :: XMMS (Result PlaybackStatus)
broadcastPlaybackStatus = liftXMMSResult XP.broadcastPlaybackStatus
                          