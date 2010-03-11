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

module XMMS2.Client.Types.Coll
  ( module XMMS2.Client.Bindings.Types.Coll
  ) where

import XMMS2.Client.Types.Value

import XMMS2.Client.Bindings.Types.Coll
  ( Coll
  , CollType (..)
  , getColl
  , newColl
  , collNew
  , collSetIdlist
  , collAddOperand
  , collIdlistAppend
  , collParse
  , collNewIdlist )


instance ValueGet Coll where
  valueGet = getColl

instance ValueNew Coll where
  valueNew = newColl
