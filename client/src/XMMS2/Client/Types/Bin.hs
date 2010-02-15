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

module XMMS2.Client.Types.Bin
  ( module XMMS2.Client.Bindings.Types.Bin
  ) where

import XMMS2.Client.Types.Value

import XMMS2.Client.Bindings.Types.Bin
  ( Bin
  , withBin
  , makeBin
  , getBin
  , newBin )


instance ValueGet Bin where
  valueGet = getBin

instance ValueNew Bin where
  valueNew = newBin
