-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 17 Sep. 2009
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

{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module XMMS2.Client.CollBase
  ( CollPtr
  , Coll
  , withColl
  , takeColl
  , CollType
    ( TypeReference
    , TypeUnion
    , TypeIntersection
    , TypeComplement
    , TypeHas
    , TypeEquals
    , TypeMatch
    , TypeSmaller
    , TypeGreater
    , TypeIdlist
    , TypeQueue
    , TypePartyshuffle )
  , collNew
  , collSetIdlist
  , collNewIdlist
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsv" #}         

import XMMS2.Utils


data T = T
{# pointer *coll_t as CollPtr -> T #}
data Coll = Coll (ForeignPtr T)
          deriving (Show, Eq)


withColl (Coll p) = withForeignPtr p

takeColl ref p = do
  p' <- if ref then coll_ref p else return p
  fp <- newForeignPtr coll_unref p'
  return $ Coll fp

{# fun coll_ref as coll_ref
 { id `CollPtr'
 } -> `CollPtr' id #}

foreign import ccall unsafe "&xmmsv_coll_unref"
  coll_unref :: FinalizerPtr T

{# enum coll_type_t as CollType
 { underscoreToCase }
 with prefix = "XMMS_COLLECTION"
 deriving (Show) #}

collNew :: CollType -> IO Coll
collNew t = coll_new t >>= takeColl False
{# fun coll_new as coll_new
 { cFromEnum `CollType'
 } -> `CollPtr' id #}

collSetIdlist :: Coll -> [Int32] -> IO ()
collSetIdlist c i = coll_set_idlist c $ map fromIntegral i
{# fun coll_set_idlist as coll_set_idlist
 { withColl*    `Coll'
 , withZTArray* `[CUInt]'
 } -> `()' #}

collNewIdlist list = do
  c <- collNew TypeIdlist
  collSetIdlist c list
  return c
