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

module XMMS2.Client
  ( module XMMS2.Client.Exception
  , module XMMS2.Client.Types
  , module XMMS2.Client.Connection
  , module XMMS2.Client.Coll
  , module XMMS2.Client.Medialib
  , module XMMS2.Client.Playback
  , module XMMS2.Client.Playlist
  , module XMMS2.Client.Result
  , module XMMS2.Client.Stats
  ) where

import XMMS2.Client.Exception
import XMMS2.Client.Types
import XMMS2.Client.Connection
import XMMS2.Client.Coll
import XMMS2.Client.Medialib
import XMMS2.Client.Playback
import XMMS2.Client.Playlist
import XMMS2.Client.Result hiding (liftResult)
import XMMS2.Client.Stats
