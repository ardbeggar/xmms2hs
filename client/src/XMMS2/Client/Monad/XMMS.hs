-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 14 Jan. 2010
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

module XMMS2.Client.Monad.XMMS
  ( XMMS
  , runXMMS
  ) where

import Control.Monad.Reader
import XMMS2.Client.Connection (Connection)
import XMMS2.Client.Monad.Monad  


type XMMS e m = ReaderT (Connection, e) m

instance XMMSM m => XMMSM (ReaderT (Connection, e) m)
  
instance XMMSM m => XMMSCM e (ReaderT (Connection, e) m) where
  connection  = asks fst
  environment = asks snd


runXMMS f c e = runReaderT f (c, e)
