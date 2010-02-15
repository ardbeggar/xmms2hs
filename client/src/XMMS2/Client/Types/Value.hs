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
  ( module XMMS2.Client.Bindings.Types.Value
  , ValueGet (..)
  , ValueNew (..)
  ) where

import Control.Monad.Trans

import XMMS2.Client.Monad.Monad

import XMMS2.Client.Bindings.Types.Value
  ( Value
  , ValueType (..)
  , Int32
  , getType
  , getError
  , getNone
  , newNone
  , getInt
  , newInt
  , getString
  , newString )


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
