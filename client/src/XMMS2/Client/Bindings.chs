-- -*- haskell -*-

{-# LANGUAGE ForeignFunctionInterface #-}

module XMMS2.Client.Bindings
  ( Value
  , xmmsvGetInt
  , Connection
  , xmmscInit
  , xmmscConnect
  , printConnection
  , Result
  , xmmscResultWait
  , xmmscResultGetValue
  , xmmscPlaybackStart
  , xmmscPlaybackStop
  ) where

#include <xmmsclient/xmmsclient.h>

import C2HS         
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Alloc

{#pointer *xmmsv_t as Value newtype#}
{#pointer *xmmsc_connection_t as Connection newtype#}
{#pointer *xmmsc_result_t as Result newtype#}

{#fun xmmsv_get_int as ^
 {id      `Value'           ,
  alloca- `Int' peekIntConv*} -> `Int'#}

{#fun xmmsc_init as ^
 {withCString* `String'} -> `Connection' id#}

{#fun xmmsc_connect as ^
 {id `Connection' ,
  withCString* `String' } -> `Bool' #}

{#fun xmmsc_result_wait as ^
 {id `Result'} -> `()' #}

{#fun xmmsc_result_get_value as ^
 {id `Result'} -> `Value' id #}

{#fun xmmsc_playback_start as ^
 {id `Connection'} -> `Result' id #}

{#fun xmmsc_playback_stop as ^
 {id `Connection'} -> `Result' id #}

printConnection (Connection ptr) = print ptr
                                   