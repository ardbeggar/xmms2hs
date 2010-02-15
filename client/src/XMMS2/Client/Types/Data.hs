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

module XMMS2.Client.Types.Data
  ( Data
  , mkData
  , dataInt32
  , dataString
  , dataColl
  , dataBin
  , dataList
  , dataDict
  , lookupInt32
  , lookupString
  , lookupColl
  , lookupBin
  , lookupList
  , lookupDict
  ) where

import Control.Applicative
--import Control.Monad.Trans

import Data.Maybe
import qualified Data.Map as Map

import XMMS2.Client.Exception

import XMMS2.Client.Types.Value
import XMMS2.Client.Types.Coll
import XMMS2.Client.Types.Bin
import XMMS2.Client.Types.List
import XMMS2.Client.Types.Dict


class (ValueGet a, ValueNew a) => ValuePrim a where
  primInt32  :: a -> Maybe Int32
  primInt32  = const Nothing
  primString :: a -> Maybe String
  primString = const Nothing
  primColl   :: a -> Maybe Coll
  primColl   = const Nothing
  primBin    :: a -> Maybe Bin
  primBin    = const Nothing
  primList   :: a -> Maybe [Data]
  primList   = const Nothing
  primDict   :: a -> Maybe (Dict Data)
  primDict   = const Nothing


instance ValuePrim ()

instance ValuePrim Int32 where
  primInt32 = Just

instance ValuePrim String where
  primString = Just

instance ValuePrim Coll where
  primColl = Just

instance ValuePrim Bin where
  primBin = Just

instance ValuePrim [Data] where
  primList = Just

instance ValuePrim (Dict Data) where
  primDict = Just


data Data = forall a. ValuePrim a => Data a

instance ValueGet Data where
  valueGet v = do
    t <- getType v
    case t of
      TypeNone   -> Data <$> getNone v
      TypeInt32  -> Data <$> getInt v
      TypeString -> Data <$> getString v
      TypeColl   -> Data <$> getColl v
      TypeBin    -> Data <$> getBin v
      TypeList   -> Data <$> ((getList v) :: IO [Data])
      TypeDict   -> Data <$> ((getDict v) :: IO (Dict Data))
      TypeError  -> throwIO . XMMSError . fromJust =<< getError v

instance ValueNew Data where
  valueNew (Data a) = valueNew a


mkData = Data

dataInt32  (Data a) = primInt32 a
dataString (Data a) = primString a
dataColl   (Data a) = primColl a
dataBin    (Data a) = primBin a
dataList   (Data a) = primList a
dataDict   (Data a) = primDict a


lookupInt32  k d = dataInt32  =<< Map.lookup k d
lookupString k d = dataString =<< Map.lookup k d
lookupColl   k d = dataColl   =<< Map.lookup k d
lookupBin    k d = dataBin    =<< Map.lookup k d
lookupList   k d = dataList   =<< Map.lookup k d
lookupDict   k d = dataDict   =<< Map.lookup k d
