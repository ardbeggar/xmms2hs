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
  , playbackStart
  , playbackStop
  , playbackTickle
  , playbackStatus
  , signalPlaybackPlaytime
  , broadcastPlaybackStatus
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsc" #}

import XMMS2.Utils         
{# import XMMS2.Client.Connection #}  
{# import XMMS2.Client.Result #}


{# enum xmms_playback_status_t as PlaybackStatus
 { underscoreToCase }
 with prefix = "XMMS_PLAYBACK_"
 deriving (Show) #}


{# fun playback_start as ^
 { withConnection* `Connection'
 } -> `Result' peekResult* #}

{# fun playback_stop as ^
 { withConnection* `Connection'
 } -> `Result' peekResult* #}

{# fun playback_tickle as ^
 { withConnection* `Connection'
 } -> `Result' peekResult* #}

{# fun playback_status as ^
 { withConnection* `Connection'
 } -> `Result' peekResult* #}


{# fun signal_playback_playtime as ^
 { withConnection* `Connection'
 } -> `Result' peekResult* #}


{# fun broadcast_playback_status as ^
 { withConnection* `Connection'
 } -> `Result' peekResult* #}
