-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 5 Oct. 2009
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

module XMMS2.Client.Bindings.Types.Value
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
  , Int32
  , withValue
  , takeValue
  , refValue
  , getType
  , getError
  , getNone
  , newNone
  , getInt
  , newInt
  , getString
  , newString
  , get
  , raiseGetError
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsv" #}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.Maybe

import XMMS2.Utils
import XMMS2.Client.Exception


data T = T
{# pointer *t as ValuePtr -> T #}
data Value = Value (ForeignPtr T)

withValue (Value p) = withForeignPtr p

takeValue ref p = do
  p' <- if ref then xmmsv_ref p else return p
  takePtr Value xmmsv_unref p'

{# fun xmmsv_ref as xmmsv_ref
 { id `ValuePtr'
 } -> `ValuePtr' id #}

foreign import ccall unsafe "&xmmsv_unref"
  xmmsv_unref :: FunPtr (ValuePtr -> IO ())

refValue :: Value -> IO Value
refValue val = withValue val $ takeValue True


{# enum type_t as ValueType
 { underscoreToCase }
 deriving (Show) #}

{# fun get_type as ^
 { withValue* `Value'
 } -> `ValueType' cToEnum #}


getError value = do
  (ok, err) <- get_error value
  if ok
     then Just <$> peekCString err
     else return Nothing
{# fun get_error as get_error
 { withValue* `Value'
 , alloca-    `CString' peek*
 } -> `Bool' #}

getNone = get TypeNone (const $ return (True, ())) return

newNone = new_none >>= takeValue False
{# fun new_none as new_none
 {} -> `ValuePtr' id #}

getInt = get TypeInt32 get_int return
{# fun get_int as get_int
 { withValue* `Value'
 , alloca-    `Int32' peekIntConv*
 } -> `Bool' #}

newInt val = new_int val >>= takeValue False
{# fun new_int as new_int
 { cIntConv `Int32'
 } -> `ValuePtr' id #}

getString = get TypeString get_string peekCString
{# fun get_string as get_string
 { withValue* `Value'
 , alloca-    `CString' peek*
 } -> `Bool' #}

newString val = new_string val >>= takeValue False
{# fun new_string as new_string
 { withCString* `String'
 } -> `ValuePtr' id #}


get t f c v = do
  (ok, v') <- f v
  if ok then c v' else raiseGetError t v

raiseGetError t v = do
  t' <- getType v
  case t' of
    TypeError -> do
      throwIO . XMMSError . fromJust =<< getError v
    _         ->
      fail $ "type mismatch: want " ++ show t ++ ", got " ++ show t'
