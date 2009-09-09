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
  , Int32
  , ValueTypeClass (..)
  , listGetSize
  , listGet
  , getInt
  , getString
  , getList
  ) where

import XMMS2.Client.Monad.Monad
import XMMS2.Client.Value (Value, ValueType, Int32)  
import qualified XMMS2.Client.Value as XV
import Control.Monad  
import Data.Maybe
  

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

-- TODO: check for value type errors.

listGetSize val = liftIO $ XV.listGetSize val

listGet val = liftGet $ XV.listGet val

getInt = liftGet XV.getInt

getString = liftGet XV.getString

getListIter = liftGet XV.getListIter

listIterValid = liftIO . XV.listIterValid

listIterEntry = liftGet XV.listIterEntry

listIterNext = liftIO . XV.listIterNext
          
getList :: ValueTypeClass a => Value -> XMMS [a]
getList val = do
  iter <- getListIter val
  while (listIterValid iter) $ do
    entry <- listIterEntry iter
    item  <- valueToType entry
    listIterNext iter
    return item

liftGet f x = liftM fromJust $ liftIO $ f x

while c a = do
  continue <- c
  if continue
    then liftM2 (:) a (while c a)
    else return []
