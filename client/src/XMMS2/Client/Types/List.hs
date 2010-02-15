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

module XMMS2.Client.Types.List
  ( getList
  , newList
  , strictGetList
  ) where

import XMMS2.Utils

import XMMS2.Client.Types.Value
import qualified XMMS2.Client.Bindings.Types as B


instance ValueGet a => ValueGet [a] where
  valueGet = getList

instance ValueNew a => ValueNew [a] where
  valueNew = newList

getList :: ValueGet a => Value -> IO [a]
getList = getList' lazyWhile

strictGetList :: ValueGet a => Value -> IO [a]
strictGetList = getList' while

getList' while val = do
  iter <- B.getListIter val
  while (B.listIterValid iter) $ do
    entry <- B.listIterEntry iter
    B.listIterNext iter
    valueGet entry

newList list = do
  val <- B.newList
  mapM_ (\v -> valueNew v >>= B.listAppend val) list
  return val
