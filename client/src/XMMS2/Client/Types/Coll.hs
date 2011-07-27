-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 15 Feb. 2010
--
--  Copyright (C) 2009-2011 Oleg Belozeorov
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

module XMMS2.Client.Types.Coll
  ( module XMMS2.Client.Bindings.Types.Coll
  , CollS (..)
  , collGetS
  , collOperandsGet
  , collAttributesGet
  ) where

import qualified Data.Map as Map

import XMMS2.Client.Types.Value
import XMMS2.Client.Types.List
import XMMS2.Client.Types.Dict

import XMMS2.Client.Bindings.Types.Coll
  ( Coll
  , CollType (..)
  , getColl
  , newColl
  , collNew
  , collSetIdlist
  , collAddOperand
  , collIdlistAppend
  , collGetType
  , collAttributeSet
  , collUniverse
  , collParse
  , collNewIdlist )
import qualified XMMS2.Client.Bindings.Types.Coll as B


instance ValueGet Coll where
  valueGet = getColl

instance ValueNew Coll where
  valueNew = newColl


data CollS
  = Union [CollS]
  | Intersection [CollS]
  | Complement CollS
  | Equals String String
  | CollS CollType [CollS] (Dict String)
  deriving (Show)

collGetS coll = do
  t <- collGetType coll
  case t of
    -- TypeUnion -> do
    --   ops <- collOperandsGet coll >>= mapM collGetS
    --   return $ Union ops
    -- TypeIntersection -> do
    --   ops <- collOperandsGet coll >>= mapM collGetS
    --   return $ Intersection ops
    -- TypeComplement -> do
    --   [op] <- collOperandsGet coll
    --   op' <- collGetS op
    --   return $ Complement op'
    -- TypeEquals -> do
    --   ats <- collAttributesGet coll
    --   let Just field = Map.lookup "field" ats
    --       Just value = Map.lookup "value" ats
    --   return $ Equals field value
    _ -> do
      ops <- collOperandsGet coll >>= mapM collGetS
      ats <- collAttributesGet coll
      return $ CollS t ops ats

collOperandsGet :: Coll -> IO [Coll]
collOperandsGet coll = B.collOperandsGet coll >>= valueGet

collAttributesGet :: Coll -> IO (Dict String)
collAttributesGet coll = B.collAttributesGet coll >>= valueGet
