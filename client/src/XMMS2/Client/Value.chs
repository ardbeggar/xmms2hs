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
  , listGetSize
  , listGet
  , ListIter
  , getListIter
  , listIterValid
  , listIterEntry
  , listIterNext
  , Int32
  , Dict
  , propdictToDict
  , dictForeach
  , DictIter
  , getDictIter
  , dictIterValid
  , dictIterPair
  , dictIterNext
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

#include <xmmsclient/xmmsclient.h>
#include <xmms2hs-client.h>

{# context prefix = "xmmsv" #}

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

{# import XMMS2.Client.Bindings.Types.Value #}
{# import XMMS2.Client.Bindings.Types.Coll #}
{# import XMMS2.Client.Bindings.Types.Bin #}


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
  iter <- getListIter val
  while (listIterValid iter) $ do
    entry <- listIterEntry iter
    listIterNext iter
    return entry

newList list = do
  val <- new_list >>= takeValue False
  mapM_ (listAppend val) list
  return val
{# fun new_list as new_list
 {} -> `ValuePtr' id #}

{# fun list_get_size as ^
 { withValue* `Value'
 } -> `Integer' cIntConv #}

listGet l p = get TypeList ((flip list_get) p) (takeValue True) l
{# fun list_get as list_get
 { withValue* `Value'
 , cIntConv   `Integer'
 , alloca-    `ValuePtr' peek*
 } -> `Bool' #}

listAppend list val = valueNew val >>= list_append list
{#fun list_append as list_append
 { withValue* `Value'
 , withValue* `Value'
 } -> `Int' #}


data Li = Li
{# pointer *list_iter_t as ListIterPtr -> Li #}
data ListIter a = ListIter (ForeignPtr Li)

withListIter (ListIter p) = withForeignPtr p

getListIter :: Value -> IO (ListIter a)
getListIter = get TypeList get_list_iter (takePtr ListIter finalize_list_iter)
{# fun xmms2hs_get_list_iter as get_list_iter
 { withValue* `Value'
 , alloca-    `ListIterPtr' peek*
 } -> `Bool' #}

foreign import ccall unsafe "&xmms2hs_finalize_list_iter"
  finalize_list_iter :: FinalizerPtr Li


listIterEntry :: ValueGet a => ListIter a -> IO a
listIterEntry iter = do
  (ok, v') <- list_iter_entry iter
  unless ok $ throwIO InvalidIter
  valueGet =<< takeValue True v'
{# fun list_iter_entry as list_iter_entry
 { withListIter* `ListIter a'
 , alloca-       `ValuePtr' peek*
 } -> `Bool' #}

{# fun list_iter_valid as ^
 { withListIter* `ListIter a'
 } -> `Bool' #}

{# fun list_iter_next as ^
 { withListIter* `ListIter a'
 } -> `()' #}


type Dict a = Map String a

instance ValueGet a => ValueGet (Dict a) where
  valueGet = liftIO . getDict

instance ValueNew a => ValueNew (Dict a) where
  valueNew = liftIO . newDict

getDict :: ValueGet a => Value -> IO (Dict a)
getDict val = Map.fromList <$> do
  iter <- getDictIter val
  while (dictIterValid iter) $ do
    (key, val) <- dictIterPair iter
    dictIterNext iter
    return (key, val)

newDict dict = do
  val <- new_dict >>= takeValue False
  mapM_ (uncurry (dictSet val)) $ Map.toList dict
  return val
{# fun new_dict as new_dict
 {} -> `ValuePtr' id #}

dictSet dict key val = valueNew val >>= dict_set dict key
{# fun dict_set as dict_set
 { withValue*   `Value'
 , withCString* `String'
 , withValue*   `Value'
 } -> `Int' #}


data Di = Di
{# pointer *xmms2hs_dict_iter_t as DictIterPtr -> Di #}
data DictIter a = DictIter (ForeignPtr Di)

withDictIter (DictIter p) f =
  withForeignPtr p $ \p -> {# get xmms2hs_dict_iter_t->iter #} p >>= f

getDictIter :: Value -> IO (DictIter a)
getDictIter = get TypeDict get_dict_iter (takePtr DictIter finalize_dict_iter)
{# fun xmms2hs_get_dict_iter as get_dict_iter
 { withValue* `Value'
 , alloca-    `DictIterPtr' peek*
 } -> `Bool' #}

foreign import ccall unsafe "&xmms2hs_finalize_dict_iter"
  finalize_dict_iter :: FinalizerPtr Di


dictIterPair :: ValueGet a => DictIter a -> IO (String, a)
dictIterPair iter = do
  (ok, keyptr, valptr) <- dict_iter_pair iter
  unless ok $ throwIO $ InvalidIter
  key <- peekCString keyptr
  val <- valueGet =<< takeValue True valptr
  return (key, val)
{# fun dict_iter_pair as dict_iter_pair
 { withDictIter* `DictIter a'
 , alloca-       `CString'  peek*
 , alloca-       `ValuePtr' peek*
 } -> `Bool' #}

{# fun dict_iter_valid as ^
 { withDictIter* `DictIter a'
 } -> `Bool' #}

{# fun dict_iter_next as ^
 { withDictIter* `DictIter a'
 } -> `()' #}


type DictForeachFun a = CString -> ValuePtr -> Ptr () -> IO ()
type DictForeachPtr a = FunPtr (DictForeachFun a)

dictForeach :: (String -> Value -> IO ()) -> Value -> IO ()
dictForeach f d = do
  f' <- mkDictForeachPtr $
        \s v _ -> do
          s' <- peekCString s
          v' <- takeValue True v
          f s' v'
  dict_foreach d f' nullPtr
  freeHaskellFunPtr f'
{# fun dict_foreach as dict_foreach
 { withValue* `Value'
 , id         `DictForeachPtr a'
 , id         `Ptr ()'
 } -> `()' #}

foreign import ccall "wrapper"
  mkDictForeachPtr :: DictForeachFun a -> IO (DictForeachPtr a)


propdictToDict :: Value -> [String] -> IO (Value)
propdictToDict v p = propdict_to_dict v p >>= takeValue False
{# fun propdict_to_dict as propdict_to_dict
 { withValue*         `Value'
 , withCStringArray0* `[String]'
 } -> `ValuePtr' id #}


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
      iter <- getDictIter dict
      while (dictIterValid iter) $ do
        (key, raw) <- dictIterPair iter
        val        <- valueGet raw
        dictIterNext iter
        return (key, val)


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
