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
  ( collGet
  , collList
  , collIdlistFromPlaylistFile
  , collSync
  , collQueryIds
  ) where

import XMMS2.Client.Types
import XMMS2.Client.Result

import XMMS2.Client.Bindings.Connection
import qualified XMMS2.Client.Bindings.Coll as B


collGet :: Connection -> String -> String -> IO (Result Coll)
collGet xmmsc name ns =
  liftResult $ B.collGet xmmsc name ns

collList :: Connection -> String -> IO (Result [String])
collList xmmsc ns =
  liftResult $ B.collList xmmsc ns

collIdlistFromPlaylistFile :: Connection -> String -> IO (Result Coll)
collIdlistFromPlaylistFile xmmsc file =
  liftResult $ B.collIdlistFromPlaylistFile xmmsc file

collSync :: Connection -> IO (Result ())
collSync xmmsc =
  liftResult $ B.collSync xmmsc

collQueryIds ::
  Connection ->
  Coll       ->
  [String]   ->
  Int        ->
  Int        ->
  IO (Result [Int32])
collQueryIds xmmsc coll order start len = do
  order' <- valueNew order
  liftResult $ B.collQueryIds xmmsc coll order' start len
