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
  ( ValueType
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
  , withValue
  , takeValue
  , getType
  , getInt
  , getString
  , getData
  , listGetSize
  , listGet
  , ListIter
  , getListIter
  , listIterValid
  , listIterEntry
  , listIterNext
  , Int32
  , propdictToDict
  , dictForeach
  ) where

#include <xmmsclient/xmmsclient.h>

import Control.Monad
import Data.Int (Int32)
import Data.Maybe
import XMMS2.Utils

  
data Xmmsv_t = Xmmsv_t
{# pointer *xmmsv_t as ValuePtr -> Xmmsv_t #}
data Value = forall a. Value (Maybe a) (ForeignPtr Xmmsv_t)

withValue (Value _ p) = withForeignPtr p

takeValue o p = do
  f <- maybe (newForeignPtr xmmsv_unref p) (\_ -> newForeignPtr_ p) o
  return $ Value o f
foreign import ccall unsafe "&xmmsv_unref"
  xmmsv_unref :: FunPtr (ValuePtr -> IO ())


{# enum xmmsv_type_t as ValueType
 { underscoreToCase }
 with prefix = "XMMSV_"
 deriving (Show) #}

{# fun xmmsv_get_type as getType
 { withValue* `Value'
 } -> `ValueType' cToEnum #}
               

getInt v = xmmsv_get_int v >>= toMaybe_
{# fun xmmsv_get_int as xmmsv_get_int
 { withValue* `Value'              ,
   alloca-    `Int32' peekIntConv*
 } -> `Bool' #}

getString v = xmmsv_get_string v >>= toMaybe peekCString
{# fun xmmsv_get_string as xmmsv_get_string
 { withValue* `Value'         ,
   alloca-    `CString' peek*
 } -> `Bool' #}


data ValueData
  = DataNone
  | DataError String
  | DataInt32 Int32
  | DataString String
    deriving Show

getData v = do
  t <- getType v
  case t of
    TypeInt32  -> mk DataInt32 getInt
    TypeString -> mk DataString getString
    _          -> return DataNone
  where mk c g = liftM (c . fromJust) $ g v


{# fun xmmsv_list_get_size as listGetSize
 { withValue* `Value'
 } -> `Integer' cIntConv #}

listGet l p = xmmsv_list_get l p >>= toMaybe (takeValue (Just l))
{# fun xmmsv_list_get as xmmsv_list_get
 { withValue* `Value'          ,
   cIntConv   `Integer'        , 
   alloca-    `ValuePtr' peek*
 } -> `Bool' #}

{# pointer *xmmsv_list_iter_t as ListIterPtr #}

data ListIter = ListIter Value ListIterPtr

withListIter (ListIter _ p) f = f p

getListIter v = get_list_iter v >>= toMaybe (return . ListIter v)
{# fun xmmsv_get_list_iter as get_list_iter
 { withValue* `Value'             ,
   alloca-    `ListIterPtr' peek*
 } -> `Bool' #}

listIterEntry i@(ListIter v _) = list_iter_entry i >>= toMaybe (takeValue (Just v))
{# fun xmmsv_list_iter_entry as list_iter_entry
 { withListIter* `ListIter'      ,
   alloca-       `ValuePtr' peek*
 } -> `Bool' #}

{# fun xmmsv_list_iter_valid as listIterValid
 { withListIter* `ListIter'
 } -> `Bool' #}

{# fun xmmsv_list_iter_next as listIterNext
 { withListIter* `ListIter'
 } -> `()' #}


type DictForeachFun = CString -> ValuePtr -> Ptr () -> IO ()
type DictForeachPtr = FunPtr (DictForeachFun)

dictForeach d f = do
  f' <- mkDictForeachPtr $
        \s v _ -> do
          s' <- peekCString s
          v' <- takeValue (Just d) v
          f s' v'
  xmmsv_dict_foreach d f' nullPtr
  freeHaskellFunPtr f'

{# fun xmmsv_dict_foreach as xmmsv_dict_foreach
 { withValue* `Value'          ,
   id         `DictForeachPtr' ,
   id         `Ptr ()'
 } -> `()' #}
  
foreign import ccall "wrapper"
  mkDictForeachPtr :: DictForeachFun -> IO DictForeachPtr

propdictToDict v = xmmsv_propdict_to_dict v nullPtr >>= takeValue Nothing
{# fun xmmsv_propdict_to_dict as xmmsv_propdict_to_dict
 { withValue* `Value'       ,
   id         `Ptr CString'
 } -> `ValuePtr' id #}


toMaybe _ (False, _) = return Nothing
toMaybe f (True, v)  = liftM Just $ f v

toMaybe_ = toMaybe return        
