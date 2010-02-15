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

module XMMS2.Client.Types.Property
  ( Property (..)
  , PropDict
  ) where

import Control.Applicative
import Control.Monad.Trans

import XMMS2.Utils

import XMMS2.Client.Types.Value
import XMMS2.Client.Types.Dict


data Property
  = PropInt32 Int32
  | PropString String
    deriving (Eq, Show, Read)

instance ValueGet Property where
  valueGet v =
    liftIO $ do
      t <- getType v
      case t of
        TypeInt32  -> PropInt32  <$> getInt v
        TypeString -> PropString <$> getString v
        _          -> fail $ "Property.valueGet: bad type " ++ show t


type PropDict = Dict [(String, Property)]

instance ValueGet [(String, Property)] where
  valueGet v = liftIO $ valueGet v >>= getAssocs
