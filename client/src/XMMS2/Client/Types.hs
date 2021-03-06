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

module XMMS2.Client.Types
  ( module XMMS2.Client.Types.Value
  , module XMMS2.Client.Types.List
  , module XMMS2.Client.Types.Coll
  , module XMMS2.Client.Types.Bin
  , module XMMS2.Client.Types.Dict
  , module XMMS2.Client.Types.Property
  , module XMMS2.Client.Types.Data
    -- * Types
  , MediaId
  , URL
  , EncodedURL
  ) where

import XMMS2.Client.Types.Value
import XMMS2.Client.Types.Coll
import XMMS2.Client.Types.Bin
import XMMS2.Client.Types.List
import XMMS2.Client.Types.Dict
import XMMS2.Client.Types.Property
import XMMS2.Client.Types.Data


--------
-- Types

-- | A Medialib entry identifier.
type MediaId = Int32

-- | A 'String' identifying a resource (media file, playlist etc.)
-- from XMMS2 daemon's point of view.
type URL = String

-- | Same as 'URL', but encoded in XMMS2 specific format.
type EncodedURL = String
