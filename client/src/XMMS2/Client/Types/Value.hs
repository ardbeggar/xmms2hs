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

import Control.Applicative

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
  valueGet :: Value -> IO a

class ValueNew a where
  valueNew :: a -> IO Value


instance ValueGet Value where
  valueGet = return

instance ValueNew Value where
  valueNew = return


instance ValueGet () where
  valueGet = getNone

instance ValueNew () where
  valueNew = const newNone


instance ValueGet Int32 where
  valueGet = getInt

instance ValueNew Int32 where
  valueNew = newInt


instance ValueGet String where
  valueGet = getString

instance ValueNew String where
  valueNew = newString

instance ValueGet a => ValueGet (Maybe a) where
  valueGet v = do
    t <- getType v
    case t of
      TypeNone -> return Nothing
      _        -> Just <$> valueGet v
