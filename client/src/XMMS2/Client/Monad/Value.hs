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

module XMMS2.Client.Monad.Value
  ( Value
  , ValueType (..)
  , ValueData (..)
  , Int32
  , ValueClass (..)
  , listGetSize
  , listGet
  , getInt
  , getString
  , getList
  , Dict
  , getDict
  ) where

import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Utils
import XMMS2.Client.Value (Value, ValueType, ValueData, Int32)
import qualified XMMS2.Client.Value as XV
import Control.Monad  
import Data.Maybe
import Data.Map (Map, fromList)  
import Control.Monad.Error
  

class ValueClass t where
  valueGet :: Value -> XMMS t
                 

instance ValueClass () where
  valueGet _ = return ()

instance ValueClass Value where
  valueGet = return

instance ValueClass Int32 where
  valueGet = getInt

instance ValueClass String where
  valueGet = getString

instance ValueClass a => ValueClass [a] where
  valueGet = getList

instance ValueClass ValueData where
  valueGet v = liftIO $ XV.getData v


-- TODO: check for value type errors.

listGetSize val = liftIO $ XV.listGetSize val

listGet val = liftGet $ XV.listGet val

getInt = liftGet XV.getInt

getString = liftGet XV.getString

getListIter = liftGet XV.getListIter

listIterValid = liftIO . XV.listIterValid

listIterEntry = liftGet XV.listIterEntry

listIterNext = liftIO . XV.listIterNext
          
getList :: ValueClass a => Value -> XMMS [a]
getList val = do
  iter <- getListIter val
  while (listIterValid iter) $ do
    entry <- listIterEntry iter
    item  <- valueGet entry
    listIterNext iter
    return item


getDictIter = liftGet XV.getDictIter

dictIterValid = liftIO . XV.dictIterValid

dictIterPair = liftGet XV.dictIterPair

dictIterNext = liftIO . XV.dictIterNext

type Dict a = Map String a

getDict :: ValueClass a => Value -> XMMS (Dict a)
getDict val = liftM fromList $ do
  iter <- getDictIter val
  while (dictIterValid iter) $ do
    (key, raw) <- dictIterPair iter
    val        <- valueGet raw
    dictIterNext iter
    return (key, val)

instance ValueClass a => ValueClass (Dict a) where
  valueGet = getDict
