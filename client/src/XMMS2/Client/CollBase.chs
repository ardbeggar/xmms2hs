-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 17 Sep. 2009
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
  ) where

#include <xmmsclient/xmmsclient.h>

{# context prefix = "xmmsv_coll" #}         

import XMMS2.Utils


data T = T
{# pointer *t as CollPtr -> T #}
data Coll a = Coll (ForeignPtr T)


withColl (Coll p) = withForeignPtr p

takeColl r p = do
  p' <- if r then ref p else return p
  fp <- newForeignPtr unref p'
  return $ Coll fp

{# fun ref as ^
 { id `CollPtr'
 } -> `CollPtr' id #}

foreign import ccall unsafe "&xmmsv_coll_unref"
  unref :: FinalizerPtr T


instance Eq (Coll a) where
  _ == _ = False

instance Show (Coll a) where
  show _ = "<<Coll>>"


{# enum type_t as CollType
 { underscoreToCase }
 with prefix = "XMMS_COLLECTION"
 deriving (Show) #}
