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
  , getInt
  , getString
  , getError
  , getColl
  , getList
  , newList
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
  , getDict
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
  , toInt32
  , toString
  , lookupInt32
  , lookupString
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

{# import XMMS2.Client.ValueBase #}
{# import XMMS2.Client.CollBase #}


instance ValueGet () where
  valueGet = liftIO . getNone

instance ValueNew () where
  valueNew () = liftIO newNone

getNone = get TypeNone (const $ return (True, ())) return

newNone = new_none >>= takeValue False
{# fun new_none as new_none
 {} -> `ValuePtr' id #}


instance ValueGet Int32 where
  valueGet = liftIO . getInt

instance ValueNew Int32 where
  valueNew = liftIO . newInt

getInt = get TypeInt32 get_int return
{# fun get_int as get_int
 { withValue* `Value'
 , alloca-    `Int32' peekIntConv*
 } -> `Bool' #}

newInt val = new_int val >>= takeValue False
{# fun new_int as new_int
 { cIntConv `Int32'
 } -> `ValuePtr' id #}


instance ValueGet String where
  valueGet = liftIO . getString

instance ValueNew String where
  valueNew = liftIO . newString

getString = get TypeString get_string peekCString
{# fun get_string as get_string
 { withValue* `Value'
 , alloca-    `CString' peek*
 } -> `Bool' #}

newString val = new_string val >>= takeValue False
{# fun new_string as new_string
 { withCString* `String'
 } -> `ValuePtr' id #}


getError value = do
  (ok, etext) <- get_error value
  return $ if ok then Just etext else Nothing

{# fun get_error as get_error
 { withValue* `Value'
 , alloca-    `CString' peek*
 } -> `Bool' #}


getColl v = get TypeColl get_coll (takeColl True) v
{# fun get_coll as get_coll
 { withValue* `Value'
 , alloca-    `CollPtr' peek*
 } -> `Bool' #}


instance ValueGet a => ValueGet [a] where
  valueGet = liftIO . getList

instance ValueNew a => ValueNew [a] where
  valueNew = liftIO . newList

getList :: ValueGet a => Value -> IO [a]
getList val = do
  iter <- getListIter val
  lazyGetList' iter
  where
    lazyGetList' iter =
      unsafeInterleaveIO $ do
        valid <- listIterValid iter
        if valid
           then do
             entry <- listIterEntry iter
             item  <- valueGet entry
             listIterNext iter
             rest <- lazyGetList' iter
             return $ item : rest
           else
             return []

strictGetList :: ValueGet a => Value -> IO [a]
strictGetList val = do
  iter <- getListIter val
  while (listIterValid iter) $ do
    entry <- listIterEntry iter
    item  <- valueGet entry
    listIterNext iter
    return item

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


listIterEntry :: ListIter a -> IO (Value)
listIterEntry iter = do
  (ok, v') <- list_iter_entry iter
  unless ok $ throwIO $ InvalidIter
  takeValue True v'

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
    (key, raw) <- dictIterPair iter
    val        <- valueGet raw
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


dictIterPair :: DictIter a -> IO (String, Value)
dictIterPair iter = do
  (ok, keyptr, valptr) <- dict_iter_pair iter
  unless ok $ throwIO $ InvalidIter
  key <- peekCString keyptr
  val <- takeValue True valptr
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



get t f c v = do
  (ok, v') <- f v
  if ok
     then
       c v'
     else do
       t' <- getType v
       case t' of
         TypeError -> do
           (_, p) <- get_error v
           s <- peekCString p
           throwIO $ XMMSError s
         _         ->
           throwIO $ TypeMismatch t t'


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
  primInt32 :: a -> Maybe Int32
  primInt32 = const Nothing
  primString :: a -> Maybe String
  primString = const Nothing

instance ValuePrim Int32 where
  primInt32 = Just

instance ValuePrim String where
  primString = Just

data Data = forall a. ValuePrim a => Data a

mkData = Data

toInt32 (Data a) = primInt32 a
toString (Data a) = primString a

lookupInt32  k d = toInt32  =<< Map.lookup k d
lookupString k d = toString =<< Map.lookup k d

instance ValueGet Data where
  valueGet v = liftIO $ do
    t <- getType v
    case t of
      TypeInt32  -> (Data . snd) <$> get_int v
      TypeString -> Data <$> (peekCString . snd =<< get_string v)

instance ValueNew Data where
  valueNew (Data a) = valueNew a
