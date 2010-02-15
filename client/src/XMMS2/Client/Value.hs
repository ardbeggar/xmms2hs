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

module XMMS2.Client.Value
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
  , getList
  , newList
  , getDict
  , newDict
  , strictGetList
  , Int32
  , Dict
  , Property (..)
  , PropDict
  , ValuePrim (..)
  , Data
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


instance ValueGet a => ValueGet [a] where
  valueGet = liftIO . getList

instance ValueNew a => ValueNew [a] where
  valueNew = liftIO . newList

getList :: ValueGet a => Value -> IO [a]
getList = getList' lazyWhile

strictGetList :: ValueGet a => Value -> IO [a]
strictGetList = getList' while

getList' while val = do
  iter <- L.getListIter val
  while (L.listIterValid iter) $ do
    entry <- L.listIterEntry iter
    L.listIterNext iter
    valueGet entry

newList list = do
  val <- L.newList
  mapM_ (\v -> valueNew v >>= L.listAppend val) list
  return val


type Dict a = Map String a

instance ValueGet a => ValueGet (Dict a) where
  valueGet = liftIO . getDict

instance ValueNew a => ValueNew (Dict a) where
  valueNew = liftIO . newDict

getDict :: ValueGet a => Value -> IO (Dict a)
getDict val = Map.fromList <$> do
  iter <- D.getDictIter val
  while (D.dictIterValid iter) $ do
    (key, val) <- D.dictIterPair iter
    D.dictIterNext iter
    (key, ) <$> valueGet val

newDict dict = do
  val <- D.newDict
  mapM_ (\(k, v) -> valueNew v >>= D.dictSet val k) $ Map.toList dict
  return val



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
  valueGet v =
    liftIO $ do
      dict <- valueGet v
      iter <- D.getDictIter dict
      while (D.dictIterValid iter) $ do
        (key, val) <- D.dictIterPair iter
        D.dictIterNext iter
        (key, ) <$> valueGet val


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

instance ValueGet Data where
  valueGet v = liftIO $ do
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
