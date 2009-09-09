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
  , ValueTypeClass (..)
  , listGetSize
  , listGet
  , getInt
  , getString
  , getList
  , Dict
  , getDict
  ) where

import XMMS2.Client.Monad.Monad
import XMMS2.Client.Value (Value, ValueType, ValueData, Int32)  
import qualified XMMS2.Client.Value as XV
import Control.Monad  
import Data.Maybe
import Data.Map (Map, fromList)  
import Control.Monad.Error
  

class ValueTypeClass t where
  valueToType :: Value -> XMMS t
                 

instance ValueTypeClass () where
  valueToType _ = return ()

instance ValueTypeClass Value where
  valueToType = return

instance ValueTypeClass Int32 where
  valueToType = getInt

instance ValueTypeClass String where
  valueToType = getString

instance ValueTypeClass a => ValueTypeClass [a] where
  valueToType = getList

instance ValueTypeClass ValueData where
  valueToType v = liftIO $ XV.getData v

-- TODO: check for value type errors.

listGetSize val = liftIO $ XV.listGetSize val

listGet val = liftGet "list" $ XV.listGet val

getInt = liftGet "int32" XV.getInt

getString = liftGet "string" XV.getString

getListIter = liftGet "list" XV.getListIter

listIterValid = liftIO . XV.listIterValid

listIterEntry = liftGet "list iterator" XV.listIterEntry

listIterNext = liftIO . XV.listIterNext
          
getList :: ValueTypeClass a => Value -> XMMS [a]
getList val = do
  iter <- getListIter val
  while (listIterValid iter) $ do
    entry <- listIterEntry iter
    item  <- valueToType entry
    listIterNext iter
    return item


getDictIter = liftGet "dict" XV.getDictIter

dictIterValid = liftIO . XV.dictIterValid

dictIterPair = liftGet "dict iterator" XV.dictIterPair

dictIterNext = liftIO . XV.dictIterNext

liftGet name f x = checkGet ("the value does not hold " ++ name) $ liftIO $ f x

checkGet :: String -> XMMS (Maybe a) -> XMMS a
checkGet text a = do
  a' <- a
  case a' of
    Just r  -> return r
    Nothing -> throwError text

type Dict a = Map String a

getDict :: ValueTypeClass a => Value -> XMMS (Dict a)
getDict val = liftM fromList $ do
  iter <- getDictIter val
  while (dictIterValid iter) $ do
    (key, raw) <- dictIterPair iter
    val        <- valueToType raw
    dictIterNext iter
    return (key, val)

instance ValueTypeClass a => ValueTypeClass (Dict a) where
  valueToType = getDict


while c a = do
  continue <- c
  if continue
    then liftM2 (:) a (while c a)
    else return []
