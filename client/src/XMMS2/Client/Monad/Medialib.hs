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


medialibGetInfo :: Int32 -> XMMS (Result (Dict (Dict ValueData)))
medialibGetInfo id =
  liftXMMSResult $ \xmmsc -> XM.medialibGetInfo xmmsc id
                             

medialibEntryPropertySetInt :: Int32 -> String -> Int32 -> XMMS (Result ())
medialibEntryPropertySetInt id key val = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertySetInt xmmsc id key val

medialibEntryPropertySetIntWithSource :: Int32 -> String -> String -> Int32 -> XMMS (Result ())
medialibEntryPropertySetIntWithSource id key src val = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertySetIntWithSource xmmsc id key src val

medialibEntryPropertySetStr :: Int32 -> String -> String -> XMMS (Result ())
medialibEntryPropertySetStr id key val = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertySetStr xmmsc id key val

medialibEntryPropertySetStrWithSource :: Int32 -> String -> String -> String -> XMMS (Result ())
medialibEntryPropertySetStrWithSource id key src val = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertySetStrWithSource xmmsc id key src val

medialibEntryPropertyRemove :: Int32 -> String -> XMMS (Result ())
medialibEntryPropertyRemove id key = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertyRemove xmmsc id key

medialibEntryPropertyRemoveWithSource :: Int32 -> String -> String -> XMMS (Result ())
medialibEntryPropertyRemoveWithSource id key src = 
  liftXMMSResult $ \xmmsc -> XM.medialibEntryPropertyRemoveWithSource xmmsc id key src

xformMediaBrowse :: String -> XMMS (Result [Dict ValueData])
xformMediaBrowse url = 
  liftXMMSResult $ \xmmsc -> XM.xformMediaBrowse xmmsc url
  

broadcastMedialibEntryChanged :: XMMS (Result Int32)
broadcastMedialibEntryChanged =
  liftXMMSResult XM.broadcastMedialibEntryChanged
