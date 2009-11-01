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
  , medialibGetInfo
  , medialibGetId
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


medialibAddEntry :: MonadXMMS m => String -> m (Result ())
medialibAddEntry url = liftXMMSResult $ \xmmsc -> XM.medialibAddEntry xmmsc url
                       
medialibGetInfo :: MonadXMMS m => Int32 -> m (Result (Dict (Dict ValueData)))
medialibGetInfo id =
  liftXMMSResult $ \xmmsc -> XM.medialibGetInfo xmmsc id
                             
medialibGetId :: MonadXMMS m => String -> m (Result Int32)
medialibGetId url =
  liftXMMSResult $ \xmmsc -> XM.medialibGetId xmmsc url

medialibEntryPropertySetInt :: MonadXMMS m => Int32 -> String -> Int32 -> m (Result ())
medialibEntryPropertySetInt id key val = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertySetInt xmmsc id key val

medialibEntryPropertySetIntWithSource :: MonadXMMS m => Int32 -> String -> String -> Int32 -> m (Result ())
medialibEntryPropertySetIntWithSource id key src val = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertySetIntWithSource xmmsc id key src val

medialibEntryPropertySetStr :: MonadXMMS m => Int32 -> String -> String -> m (Result ())
medialibEntryPropertySetStr id key val = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertySetStr xmmsc id key val

medialibEntryPropertySetStrWithSource :: MonadXMMS m => Int32 -> String -> String -> String -> m (Result ())
medialibEntryPropertySetStrWithSource id key src val = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertySetStrWithSource xmmsc id key src val

medialibEntryPropertyRemove :: MonadXMMS m => Int32 -> String -> m (Result ())
medialibEntryPropertyRemove id key = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertyRemove xmmsc id key

medialibEntryPropertyRemoveWithSource :: MonadXMMS m => Int32 -> String -> String -> m (Result ())
medialibEntryPropertyRemoveWithSource id key src = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertyRemoveWithSource xmmsc id key src

xformMediaBrowse :: MonadXMMS m => String -> m (Result [Dict ValueData])
xformMediaBrowse url = 
  liftXMMSResult $ \xmmsc -> XM.xformMediaBrowse xmmsc url
  

broadcastMedialibEntryChanged :: MonadXMMS m => m (Result Int32)
broadcastMedialibEntryChanged =
  liftXMMSResult XM.broadcastMedialibEntryChanged
