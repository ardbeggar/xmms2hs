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
  , ValueClass (..)
  , Int32
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
import XMMS2.Client.Value (Value, ValueType, ValueData, ValueClass, Int32, Dict)
import qualified XMMS2.Client.Value as XV
import Control.Monad  
import Data.Maybe
import Data.Map (Map, fromList)  
import Control.Monad.Error
  

listGetSize = liftIO . XV.listGetSize

listGet val = liftIO . XV.listGet val

getInt = liftIO . XV.getInt

getString = liftIO . XV.getString

getList = liftIO . XV.getList

getListIter = liftIO . XV.getListIter

listIterValid = liftIO . XV.listIterValid

listIterEntry = liftIO . XV.listIterEntry

listIterNext = liftIO . XV.listIterNext

getDict = liftIO . XV.getDict          
          
getDictIter = liftIO . XV.getDictIter

dictIterValid = liftIO . XV.dictIterValid

dictIterPair = liftIO . XV.dictIterPair

dictIterNext = liftIO . XV.dictIterNext

