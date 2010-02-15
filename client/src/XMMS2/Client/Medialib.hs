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

module XMMS2.Client.Medialib
  ( BrowseEntry (..)
  , medialibAddEntry
  , medialibAddEntryFull
  , medialibAddEntryEncoded
  , medialibGetInfo
  , medialibGetId
  , medialibGetIdEncoded
  , medialibEntryPropertySet
  , medialibEntryPropertyRemove
  , xformMediaBrowse
  , broadcastMedialibEntryChanged
  ) where

import Control.Applicative
import Control.Monad

import XMMS2.Client.Types
import XMMS2.Client.Result

import XMMS2.Client.Bindings.Connection
import qualified XMMS2.Client.Bindings.Medialib as B


data BrowseEntry
  = BrowseEntry { entryPath  :: String
                , entryIsDir :: Bool }

instance ValueGet BrowseEntry where
  valueGet v = do
    dict <- valueGet v
    maybe (fail "not a browse entry") return $ do
      path <- lookupString "realpath" dict `mplus` lookupString "path" dict
      dirp <- (1 ==) <$> lookupInt32 "isdir" dict
      return BrowseEntry { entryPath = path, entryIsDir = dirp }


medialibAddEntry :: Connection -> String -> IO (Result ())
medialibAddEntry xmmsc url =
  liftResult $ B.medialibAddEntry xmmsc url

medialibAddEntryFull :: Connection -> String -> [String] -> IO (Result ())
medialibAddEntryFull xmmsc url args =
  liftResult $ B.medialibAddEntryFull xmmsc url =<< valueNew args

medialibAddEntryEncoded :: Connection -> String -> IO (Result ())
medialibAddEntryEncoded xmmsc url =
  liftResult $ B.medialibAddEntryEncoded xmmsc url

medialibGetInfo :: Connection -> Int32 -> IO (Result PropDict)
medialibGetInfo xmmsc id =
  liftResult $ B.medialibGetInfo xmmsc id

medialibGetId :: Connection -> String -> IO (Result Int32)
medialibGetId xmmsc url =
  liftResult $ B.medialibGetId xmmsc url

medialibGetIdEncoded :: Connection -> String -> IO (Result Int32)
medialibGetIdEncoded xmmsc url =
  liftResult $ B.medialibGetIdEncoded xmmsc url

medialibEntryPropertySet ::
  Connection             ->
  Int32                  ->
  Maybe String           ->
  String                 ->
  Property               ->
  IO (Result ())
medialibEntryPropertySet xmmsc id (Just src) key (PropInt32 val) =
  liftResult $ B.medialibEntryPropertySetIntWithSource xmmsc id src key val
medialibEntryPropertySet xmmsc id Nothing key (PropInt32 val) =
  liftResult $ B.medialibEntryPropertySetInt xmmsc id key val
medialibEntryPropertySet xmmsc id (Just src) key (PropString val) =
  liftResult $ B.medialibEntryPropertySetStrWithSource xmmsc id src key val
medialibEntryPropertySet xmmsc id Nothing key (PropString val) =
  liftResult $ B.medialibEntryPropertySetStr xmmsc id key val

medialibEntryPropertyRemove ::
  Connection                ->
  Int32                     ->
  Maybe String              ->
  String                    ->
  IO (Result ())
medialibEntryPropertyRemove xmmsc id (Just src) key =
  liftResult $ B.medialibEntryPropertyRemoveWithSource xmmsc id src key
medialibEntryPropertyRemove xmmsc id Nothing key =
  liftResult $ B.medialibEntryPropertyRemove xmmsc id key

xformMediaBrowse :: Connection -> String -> IO (Result [BrowseEntry])
xformMediaBrowse xmmsc url =
  liftResult $ B.xformMediaBrowse xmmsc url


broadcastMedialibEntryChanged :: Connection -> IO (Result Int32)
broadcastMedialibEntryChanged =
  liftResult . B.broadcastMedialibEntryChanged
