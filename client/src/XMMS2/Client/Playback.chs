-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 1 Sep. 2009
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

module XMMS2.Client.Playback
  ( PlaybackStatus (..)
  , start
  , stop
  , tickle
  , status
  , signalPlaybackPlaytime
  , broadcastPlaybackStatus
  ) where

#include <xmmsclient/xmmsclient.h>

import XMMS2.Utils         
{# import XMMS2.Client.Connection #}  
{# import XMMS2.Client.Result #}


{# enum xmms_playback_status_t as PlaybackStatus
 { underscoreToCase }
 with prefix = "XMMS_PLAYBACK_"
 deriving (Show) #}


{# fun xmmsc_playback_start as start
 { withConnection* `Connection'
 } -> `Result' peekResult* #}

{# fun xmmsc_playback_stop as stop
 { withConnection* `Connection'
 } -> `Result' peekResult* #}

{# fun xmmsc_playback_tickle as tickle
 { withConnection* `Connection'
 } -> `Result' peekResult* #}

{# fun xmmsc_playback_status as status
 { withConnection* `Connection'
 } -> `Result' peekResult* #}


{# fun xmmsc_signal_playback_playtime as signalPlaybackPlaytime
 { withConnection* `Connection'
 } -> `Result' peekResult* #}


{# fun xmmsc_broadcast_playback_status as broadcastPlaybackStatus
 { withConnection* `Connection'
 } -> `Result' peekResult* #}
