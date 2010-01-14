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
  ( medialibAddEntry
  , medialibAddEntryFull
  , medialibAddEntryEncoded
  , medialibGetInfo
  , medialibGetId
  , medialibGetIdEncoded
  , medialibEntryPropertySetInt
  , medialibEntryPropertySetIntWithSource
  , medialibEntryPropertySetStr
  , medialibEntryPropertySetStrWithSource
  , medialibEntryPropertyRemove
  , medialibEntryPropertyRemoveWithSource
  , xformMediaBrowse
  , broadcastMedialibEntryChanged
  ) where

import Data.Int (Int32)
import XMMS2.Client.Monad.Value
import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Result
import qualified XMMS2.Client.Medialib as XM


medialibAddEntry url = liftXMMSResult $ \xmmsc -> XM.medialibAddEntry xmmsc url

medialibAddEntryFull url args = liftXMMSResult $ \xmmsc -> XM.medialibAddEntryFull xmmsc url args

medialibAddEntryEncoded url = liftXMMSResult $ \xmmsc -> XM.medialibAddEntryEncoded xmmsc url
                       
medialibGetInfo id =
  liftXMMSResult $ \xmmsc -> XM.medialibGetInfo xmmsc id
                             
medialibGetId url =
  liftXMMSResult $ \xmmsc -> XM.medialibGetId xmmsc url

medialibGetIdEncoded url =
  liftXMMSResult $ \xmmsc -> XM.medialibGetIdEncoded xmmsc url

medialibEntryPropertySetInt id key val = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertySetInt xmmsc id key val

medialibEntryPropertySetIntWithSource id key src val = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertySetIntWithSource xmmsc id key src val

medialibEntryPropertySetStr id key val = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertySetStr xmmsc id key val

medialibEntryPropertySetStrWithSource id key src val = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertySetStrWithSource xmmsc id key src val

medialibEntryPropertyRemove id key = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertyRemove xmmsc id key

medialibEntryPropertyRemoveWithSource id key src = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertyRemoveWithSource xmmsc id key src

xformMediaBrowse url = 
  liftXMMSResult $ \xmmsc -> XM.xformMediaBrowse xmmsc url
  

broadcastMedialibEntryChanged =
  liftXMMSResult XM.broadcastMedialibEntryChanged
