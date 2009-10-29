-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 1 Sep. 2009
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

module XMMS2.Client.Value
  ( Mutable
  , Immutable
  , ValueType
    ( TypeNone
    , TypeError
    , TypeInt32
    , TypeString
    , TypeColl
    , TypeBin
    , TypeList
    , TypeDict )
  , ValueData (..)
  , ValuePtr
  , Value
  , ValueClass (..)
  , withValue
  , takeValue
  , getType
  , getInt
  , getString
  , getError
  , getColl
  , getData
  , getList
  , lazyGetList
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
  ) where

#include <xmmsclient/xmmsclient.h>
#include <xmms2hs-client.h>         

{# context prefix = "xmmsv" #}         

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Exception  
import Data.Int (Int32)
import Data.Maybe
import Data.Map (Map, fromList)
import System.IO.Unsafe  
import XMMS2.Utils
import XMMS2.Client.Exception
{# import XMMS2.Client.ValueBase #}
{# import XMMS2.Client.CollBase #}


instance (ValueClass a) () where
  valueGet v = do
    t <- liftIO $ getType v
    case t of
      TypeError -> do
        (_, p) <- liftIO $ get_error v
        s <- liftIO $ peekCString p
        throwM $ XMMSError s
      _ ->
        return ()


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

  
getInt = get TypeInt32 get_int return
{# fun get_int as get_int
 { withValue* `Value a'
 , alloca-    `Int32' peekIntConv*
 } -> `Bool' #}

instance ValueClass a Int32 where
  valueGet = liftIO . getInt


instance ValueClass a String where
  valueGet = liftIO . getString

getString = get TypeString get_string peekCString
{# fun get_string as get_string
 { withValue* `Value a'
 , alloca-    `CString' peek*
 } -> `Bool' #}

getError = get TypeError get_error peekCString
{# fun get_error as get_error
 { withValue* `Value a'
 , alloca-    `CString' peek*
 } -> `Bool' #}

getColl v = get TypeColl get_coll (takeColl False) v
{# fun get_coll as get_coll
 { withValue* `Value a'
 , alloca-    `CollPtr' peek*
 } -> `Bool' #}


data ValueData
  = DataNone
  | DataError String
  | DataInt32 Int32
  | DataString String
  | forall a. DataColl (Coll a)

instance Eq ValueData where
  DataError  e1 == DataError  e2 = e1 == e2
  DataInt32  i1 == DataInt32  i2 = i1 == i2
  DataString s1 == DataString s2 = s1 == s2
  _ == _ = False
           
getData ::  Value a -> IO ValueData
getData v = do
  t <- getType v
  case t of
    TypeInt32  -> mk DataInt32 getInt
    TypeString -> mk DataString getString
    TypeColl   -> mk DataColl getColl
    _          -> return DataNone
  where mk c g = liftM c $ g v

instance ValueClass a ValueData where
  valueGet = liftIO . getData
                 


{# fun list_get_size as listGetSize
 { withValue* `Value a'
 } -> `Integer' cIntConv #}

listGet l p = get TypeList ((flip list_get) p) (takeValue True) l
{# fun list_get as list_get
 { withValue* `Value a'
 , cIntConv   `Integer'
 , alloca-    `ValuePtr' peek*
 } -> `Bool' #}


data Li = Li
{# pointer *list_iter_t as ListIterPtr -> Li #}
data ListIter a = ListIter (ForeignPtr Li)

withListIter (ListIter p) = withForeignPtr p

getListIter :: Value a -> IO (ListIter a)
getListIter v = get TypeList get_list_iter (liftM ListIter . newForeignPtr finalize_list_iter) v

{# fun xmms2hs_get_list_iter as get_list_iter
 { withValue* `Value a'
 , alloca-    `ListIterPtr' peek*
 } -> `Bool' #}

foreign import ccall unsafe "&xmms2hs_finalize_list_iter"
  finalize_list_iter :: FinalizerPtr Li


listIterEntry :: ListIter a -> IO (Value a)
listIterEntry iter = do
  (ok, v') <- list_iter_entry iter
  unless ok $ throwIO $ InvalidIter
  takeValue True v'
{# fun list_iter_entry as list_iter_entry
 { withListIter* `ListIter a'
 , alloca-       `ValuePtr' peek*
 } -> `Bool' #}

{# fun list_iter_valid as listIterValid
 { withListIter* `ListIter a'
 } -> `Bool' #}

{# fun list_iter_next as listIterNext
 { withListIter* `ListIter a'
 } -> `()' #}


instance ValueClass Immutable a => ValueClass Immutable [a] where
  valueGet = liftIO . lazyGetList

getList :: ValueClass b a => Value b -> IO [a]
getList val = do
  iter <- getListIter val
  while (listIterValid iter) $ do
    entry <- listIterEntry iter
    item  <- valueGet entry
    listIterNext iter
    return item

lazyGetList :: ValueClass Immutable a => Value Immutable -> IO [a]
lazyGetList val = do
  iter <- getListIter val
  lazyGetList' iter
  where lazyGetList' iter =
          unsafeInterleaveIO $ do
            valid <- listIterValid iter
            if valid
               then do
                 entry <- listIterEntry iter
                 item  <- valueGet entry
                 listIterNext iter
                 liftM (item :) $ lazyGetList' iter
               else
                 return []



type Dict a = Map String a

instance ValueClass Immutable a => ValueClass Immutable (Dict a) where
  valueGet = liftIO . getDict

getDict :: ValueClass b a => Value b -> IO (Dict a)
getDict val = liftM fromList $ do
  iter <- getDictIter val
  while (dictIterValid iter) $ do
    (key, raw) <- dictIterPair iter
    val        <- valueGet raw
    dictIterNext iter
    return (key, val)



data Di = Di
{# pointer *xmms2hs_dict_iter_t as DictIterPtr -> Di #}
data DictIter a = DictIter (ForeignPtr Di)

withDictIter (DictIter p) f =
  withForeignPtr p $ \p -> {# get xmms2hs_dict_iter_t->iter #} p >>= f
    

getDictIter :: Value a -> IO (DictIter a)
getDictIter v = get TypeDict get_dict_iter (liftM DictIter . newForeignPtr finalize_dict_iter) v

{# fun xmms2hs_get_dict_iter as get_dict_iter
 { withValue* `Value a'
 , alloca-    `DictIterPtr' peek*
 } -> `Bool' #}

foreign import ccall unsafe "&xmms2hs_finalize_dict_iter"
  finalize_dict_iter :: FinalizerPtr Di

dictIterPair :: DictIter a -> IO (String, Value a)
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

{# fun dict_iter_valid as dictIterValid
 { withDictIter* `DictIter a'
 } -> `Bool' #}

{# fun dict_iter_next as dictIterNext
 { withDictIter* `DictIter a'
 } -> `()' #}


type DictForeachFun a = CString -> ValuePtr -> Ptr () -> IO ()
type DictForeachPtr a = FunPtr (DictForeachFun a)

dictForeach :: (String -> Value a -> IO ()) -> Value a -> IO ()
dictForeach f d = do
  f' <- mkDictForeachPtr $
        \s v _ -> do
          s' <- peekCString s
          v' <- takeValue True v
          f s' v'
  dict_foreach d f' nullPtr
  freeHaskellFunPtr f'
                    
{# fun dict_foreach as dict_foreach
 { withValue* `Value a'
 , id         `DictForeachPtr a'
 , id         `Ptr ()'
 } -> `()' #}
  
foreign import ccall "wrapper"
  mkDictForeachPtr :: DictForeachFun a -> IO (DictForeachPtr a)

propdictToDict :: Value a -> [String] -> IO (Value a)                      
propdictToDict v p = propdict_to_dict v p >>= takeValue False
{# fun propdict_to_dict as propdict_to_dict
 { withValue*         `Value a'
 , withCStringArray0* `[String]'
 } -> `ValuePtr' id #}

