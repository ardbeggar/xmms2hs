-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 1 Sep. 2009
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

module XMMS2.Client.Types.Value
  ( ValueType
    ( TypeNone
    , TypeError
    , TypeInt32
    , TypeString
    , TypeColl
    , TypeBin
    , TypeList
    , TypeDict )
  , ValuePtr
  , Value
  , ValueGet (..)
  , ValueNew (..)
  , withValue
  , takeValue
  , getType
  , getError
  , getNone
  , newNone
  , getInt
  , newInt
  , getString
  , newString
  , getColl
  , newColl
  , Bin
  , withBin
  , makeBin
  , getBin
  , newBin
  , Int32
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.CatchIO

import Data.Int (Int32)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import System.IO.Unsafe

import XMMS2.Utils
import XMMS2.Client.Exception
import XMMS2.Client.Monad.Monad

import XMMS2.Client.Bindings.Types.Value
import XMMS2.Client.Bindings.Types.Coll
import XMMS2.Client.Bindings.Types.Bin
import qualified XMMS2.Client.Bindings.Types.List as L
import qualified XMMS2.Client.Bindings.Types.Dict as D


class ValueGet a where
  valueGet :: XMMSM m => Value -> m a

class ValueNew a where
  valueNew :: XMMSM m => a -> m Value


instance ValueGet Value where
  valueGet = return

instance ValueNew Value where
  valueNew = return


instance ValueGet () where
  valueGet = liftIO . getNone

instance ValueNew () where
  valueNew () = liftIO newNone


instance ValueGet Int32 where
  valueGet = liftIO . getInt

instance ValueNew Int32 where
  valueNew = liftIO . newInt


instance ValueGet String where
  valueGet = liftIO . getString

instance ValueNew String where
  valueNew = liftIO . newString


instance ValueGet Coll where
  valueGet = liftIO . getColl

instance ValueNew Coll where
  valueNew = liftIO . newColl


instance ValueGet Bin where
  valueGet = liftIO . getBin

instance ValueNew Bin where
  valueNew = liftIO . newBin
