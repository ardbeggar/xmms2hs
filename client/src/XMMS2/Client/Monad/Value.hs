-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 8 Sep. 2009
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

module XMMS2.Client.Monad.Value
  ( Value
  , ValueType (..)
  , ValueGet (..)
  , ValueNew (..)
  , Int32
  , getInt
  , getString
  , getList
  , newList
  , Dict
  , getDict
  ) where

import Control.Monad
import Control.Monad.Error

import Data.Maybe
import Data.Map (Map, fromList)

import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Utils

import XMMS2.Client.Value
  ( Value
  , ValueType
  , ValueGet
  , ValueNew
  , Int32
  , Dict
  , Property
  , PropDict )
import qualified XMMS2.Client.Value as XV


getInt = liftIO . XV.getInt

getString = liftIO . XV.getString

getList = liftIO . XV.getList

newList = liftIO . XV.newList

getDict = liftIO . XV.getDict

