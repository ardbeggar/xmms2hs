-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 5 Oct. 2009
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

module XMMS2.Client.ValueBase
  ( ValueType (..)
  , ValuePtr
  , Value
  , withValue
  , takeValue
  , refValue
  , getType
  , ValueClass (..)
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsv" #}         

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Exception  
import Data.Maybe
import XMMS2.Utils  


data T = T
{# pointer *t as ValuePtr -> T #}
data Value = Value (ForeignPtr T)

withValue (Value p) = withForeignPtr p

takeValue ref p = do
  p' <- if ref then xmmsv_ref p else return p
  fp <- newForeignPtr xmmsv_unref p'
  return $ Value fp

refValue :: Value -> IO Value
refValue val = withValue val $ takeValue True

{# fun xmmsv_ref as xmmsv_ref
 { id `ValuePtr'
 } -> `ValuePtr' id #}

foreign import ccall unsafe "&xmmsv_unref"
  xmmsv_unref :: FunPtr (ValuePtr -> IO ())



{# enum type_t as ValueType
 { underscoreToCase }
 deriving (Show) #}


{# fun get_type as ^
 { withValue* `Value'
 } -> `ValueType' cToEnum #}
               

class ValueClass t where
  valueGet :: (MonadIO m, MonadException m) => Value -> m t
  valueNew :: (MonadIO m, MonadException m) => t -> m Value                 

instance ValueClass Value where
  valueGet = return
  valueNew = return

