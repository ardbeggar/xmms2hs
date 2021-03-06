-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 1 Sep. 2009
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

module XMMS2.Client.Bindings.Playback
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

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsc" #}

import XMMS2.Utils

{# import XMMS2.Client.Bindings.Connection #}
{# import XMMS2.Client.Bindings.Result #}


{# enum xmms_playback_status_t as PlaybackStatus
 { underscoreToCase }
 with prefix = "XMMS_PLAYBACK_"
 deriving (Eq, Show) #}

{# enum xmms_playback_seek_mode_t as SeekMode
 { underscoreToCase }
 with prefix = "XMMS_PLAYBACK_"
 deriving (Eq, Show) #}


{# fun playback_stop as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}

{# fun playback_tickle as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}

{# fun playback_start as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}

{# fun playback_pause as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}

{# fun playback_current_id as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}

{# fun playback_seek_ms as ^
 { withConnection* `Connection'
 , cIntConv        `Int32'
 , cFromEnum       `SeekMode'
 } -> `Result' takeResult* #}

{# fun playback_seek_samples as ^
 { withConnection* `Connection'
 , cIntConv        `Int32'
 , cFromEnum       `SeekMode'
 } -> `Result' takeResult* #}

{# fun playback_playtime as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}

{# fun playback_status as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}

{# fun playback_volume_set as ^
 { withConnection* `Connection'
 , withCString*    `String'
 , cIntConv        `Int'
 } -> `Result' takeResult* #}

{# fun playback_volume_get as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}


{# fun broadcast_playback_volume_changed as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}

{# fun broadcast_playback_status as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}

{# fun broadcast_playback_current_id as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}


{# fun signal_playback_playtime as ^
 { withConnection* `Connection'
 } -> `Result' takeResult* #}
