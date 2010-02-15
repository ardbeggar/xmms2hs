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

module XMMS2.Client.Types.Dict
  ( Dict
  , getDict
  , getAssocs
  , newDict
  ) where

import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map

import XMMS2.Utils

import XMMS2.Client.Types.Value
import qualified XMMS2.Client.Bindings.Types as B


type Dict a = Map String a

instance ValueGet a => ValueGet (Dict a) where
  valueGet = getDict

instance ValueNew a => ValueNew (Dict a) where
  valueNew = newDict

getDict :: ValueGet a => Value -> IO (Dict a)
getDict val = Map.fromList <$> getAssocs val

getAssocs :: ValueGet a => Value -> IO [(String, a)]
getAssocs val = do
  iter <- B.getDictIter val
  while (B.dictIterValid iter) $ do
    (key, val) <- B.dictIterPair iter
    B.dictIterNext iter
    (key, ) <$> valueGet val

newDict dict = do
  val <- B.newDict
  mapM_ (\(k, v) -> valueNew v >>= B.dictSet val k) $ Map.toList dict
  return val
