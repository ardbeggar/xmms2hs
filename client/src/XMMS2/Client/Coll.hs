-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 15 Feb. 2010
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

module XMMS2.Client.Coll
  ( CollectionChangedActions (..)
  , CollectionChange (..)
  , collGet
  , collList
  , collSave
  , collRemove
  , collRename
  , collIdlistFromPlaylistFile
  , collSync
  , collQueryIds
  , broadcastCollectionChanged
  ) where

import Control.Applicative

import XMMS2.Client.Types
import XMMS2.Client.Result

import XMMS2.Client.Bindings.Connection
import XMMS2.Client.Bindings.Coll (CollectionChangedActions (..))
import qualified XMMS2.Client.Bindings.Coll as B


data CollectionChange
  = CollectionAdd
    { namespace :: String
    , name      :: String }
  | CollectionUpdate
    { namespace :: String
    , name      :: String }
  | CollectionRename
    { namespace :: String
    , name      :: String
    , newName   :: String }
  | CollectionRemove
    { namespace :: String
    , name      :: String }
  deriving (Show)

instance ValueGet CollectionChange where
  valueGet v = do
    dict <- valueGet v
    maybe (fail "not a collection change notification") return $ do
      namespace <- lookupString "namespace" dict
      name      <- lookupString "name" dict
      action    <- (toEnum . fromIntegral) <$> lookupInt32 "type" dict
      case action of
        CollectionChangedAdd    ->
          return CollectionAdd { namespace = namespace
                               , name      = name }
        CollectionChangedUpdate ->
          return CollectionUpdate { namespace = namespace
                                  , name      = name }
        CollectionChangedRename -> do
          newName <- lookupString "newname" dict
          return CollectionRename { namespace = namespace
                                  , name      = name
                                  , newName   = newName }
        CollectionChangedRemove ->
          return CollectionRemove { namespace = namespace
                                  , name      = name }


collGet :: Connection -> String -> String -> IO (Result Default Coll)
collGet xmmsc name ns =
  liftResult $ B.collGet xmmsc name ns

collList :: Connection -> String -> IO (Result Default [String])
collList xmmsc ns =
  liftResult $ B.collList xmmsc ns

collSave :: Connection -> Coll -> String -> String -> IO (Result Default ())
collSave xmmsc coll name ns =
  liftResult $ B.collSave xmmsc coll name ns

collRemove :: Connection -> String -> String -> IO (Result Default ())
collRemove xmmsc name ns =
  liftResult $ B.collRemove xmmsc name ns

collRename :: Connection -> String -> String -> String -> IO (Result Default ())
collRename xmmsc from to ns =
  liftResult $ B.collRename xmmsc from to ns

collIdlistFromPlaylistFile :: Connection -> String -> IO (Result Default Coll)
collIdlistFromPlaylistFile xmmsc file =
  liftResult $ B.collIdlistFromPlaylistFile xmmsc file

collSync :: Connection -> IO (Result Default ())
collSync xmmsc =
  liftResult $ B.collSync xmmsc

collQueryIds ::
  Connection ->
  Coll       ->
  [String]   ->
  Int        ->
  Int        ->
  IO (Result Default [Int32])
collQueryIds xmmsc coll order start len = do
  order' <- valueNew order
  liftResult $ B.collQueryIds xmmsc coll order' start len


broadcastCollectionChanged :: Connection -> IO (Result Broadcast CollectionChange)
broadcastCollectionChanged = liftResult . B.broadcastCollectionChanged
