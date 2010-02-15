-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 17 Sep. 2009
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

module XMMS2.Client.Monad.Coll
  ( Coll
  , getColl
  , collIdlistFromPlaylistFile
  , collSync
  , collNew
  , collSetIdlist
  , collIdlistAppend
  , collNewIdlist
  ) where

import Control.Monad.Trans
import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Result
import XMMS2.Client.Monad.Utils
import XMMS2.Client.Monad.Value
import XMMS2.Client.Types (Coll)
import qualified XMMS2.Client.Coll as XC
import qualified XMMS2.Client.Types as XV


getColl = liftIO . XV.getColl


collIdlistFromPlaylistFile name =
  liftXMMS $ \xmmsc -> XC.collIdlistFromPlaylistFile xmmsc name

collSync = liftXMMS $ \xmmsc -> XC.collSync xmmsc


collNew = liftIO . XV.collNew

collSetIdlist c list = liftIO $ XV.collSetIdlist c list


collIdlistAppend c id = liftIO $ XV.collIdlistAppend c id


collNewIdlist = liftIO . XV.collNewIdlist
