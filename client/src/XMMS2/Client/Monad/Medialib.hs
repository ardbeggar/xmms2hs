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

module XMMS2.Client.Monad.Medialib
  ( medialibGetInfo
  , broadcastMedialibEntryChanged
  ) where

import Data.Int (Int32)
import XMMS2.Client.Monad.Value
import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Result
import qualified XMMS2.Client.Medialib as XM


medialibGetInfo :: Int32 -> XMMS (Result (Dict (Dict ValueData)))
medialibGetInfo id =
  liftXMMSResult $ \xmmsc -> XM.medialibGetInfo xmmsc id
  

broadcastMedialibEntryChanged :: XMMS (Result Int32)
broadcastMedialibEntryChanged =
  liftXMMSResult XM.broadcastMedialibEntryChanged
