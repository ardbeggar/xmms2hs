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

module XMMS2.Client.ValueBase
  ( ValueType (..)
  , ValuePtr
  , Value
  , withValue
  , takeValue
  , refValue
  , getType
  , ValueGet (..)
  , ValueNew (..)
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsv" #}

import Control.Monad
import Control.Monad.Trans

import Data.Maybe

import XMMS2.Utils
import XMMS2.Client.Monad.Monad


data T = T
{# pointer *t as ValuePtr -> T #}
data Value = Value (ForeignPtr T)

withValue (Value p) = withForeignPtr p

takeValue ref p = do
  p' <- if ref then xmmsv_ref p else return p
  fp <- newForeignPtr xmmsv_unref p'
  return $ Value fp
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


class ValueGet a where
  valueGet :: XMMSM m => Value -> m a

class ValueNew a where
  valueNew :: XMMSM m => a -> m Value

instance ValueGet Value where
  valueGet = return

instance ValueNew Value where
  valueNew = return

