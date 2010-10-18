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
  , medialibImportPath
  , medialibImportPathEncoded
  , medialibRehash
  , medialibGetId
  , medialibGetIdEncoded
  , medialibRemoveEntry
  , medialibMoveEntry
  , medialibEntryPropertySet
  , medialibEntryPropertyRemove
  , xformMediaBrowse
  , xformMediaBrowseEncoded
  , broadcastMedialibEntryChanged
  , broadcastMedialibEntryAdded
  ) where

import Control.Applicative
import Control.Monad

import XMMS2.Client.Types
import XMMS2.Client.Result

import XMMS2.Client.Bindings.Connection
import qualified XMMS2.Client.Bindings.Medialib as B


data BrowseEntry
  = BrowseEntry
    { entryPath  :: EncodedURL
    , entryIsDir :: Bool
    }

instance ValueGet BrowseEntry where
  valueGet v = do
    dict <- valueGet v
    maybe (fail "not a browse entry") return $ do
      path <- lookupString "realpath" dict <|> lookupString "path" dict
      dirp <- (1 ==) <$> lookupInt32 "isdir" dict
      return BrowseEntry { entryPath = path, entryIsDir = dirp }


medialibAddEntry :: Connection -> URL -> IO (Result Default ())
medialibAddEntry xmmsc url =
  liftResult $ B.medialibAddEntry xmmsc url

medialibAddEntryFull :: Connection -> URL -> Dict String -> IO (Result Default ())
medialibAddEntryFull xmmsc url args =
  liftResult $ B.medialibAddEntryFull xmmsc url =<< valueNew args

medialibAddEntryEncoded :: Connection -> EncodedURL -> IO (Result Default ())
medialibAddEntryEncoded xmmsc url =
  liftResult $ B.medialibAddEntryEncoded xmmsc url

medialibGetInfo :: Connection -> MediaId -> IO (Result Default PropDict)
medialibGetInfo xmmsc id =
  liftResult $ B.medialibGetInfo xmmsc id

medialibImportPath
  :: Connection
  -> URL
  -> IO (Result Default ())
medialibImportPath xmmsc url =
  liftResult $ B.medialibImportPath xmmsc url

medialibImportPathEncoded
  :: Connection
  -> EncodedURL
  -> IO (Result Default ())
medialibImportPathEncoded xmmsc url =
  liftResult $ B.medialibImportPathEncoded xmmsc url

medialibRehash
  :: Connection
  -> MediaId
  -> IO (Result Default ())
medialibRehash xmmsc id =
  liftResult $ B.medialibRehash xmmsc id

medialibGetId :: Connection -> URL -> IO (Result Default MediaId)
medialibGetId xmmsc url =
  liftResult $ B.medialibGetId xmmsc url

medialibGetIdEncoded :: Connection -> EncodedURL -> IO (Result Default MediaId)
medialibGetIdEncoded xmmsc url =
  liftResult $ B.medialibGetIdEncoded xmmsc url

medialibRemoveEntry
  :: Connection
  -> MediaId
  -> IO (Result Default ())
medialibRemoveEntry xmmsc id =
  liftResult $ B.medialibRemoveEntry xmmsc id

medialibMoveEntry
  :: Connection
  -> MediaId
  -> EncodedURL
  -> IO (Result Default ())
medialibMoveEntry xmmsc id url =
  liftResult $ B.medialibMoveEntry xmmsc id url

medialibEntryPropertySet ::
  Connection             ->
  MediaId                ->
  Maybe String           ->
  String                 ->
  Property               ->
  IO (Result Default ())
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
  MediaId                   ->
  Maybe String              ->
  String                    ->
  IO (Result Default ())
medialibEntryPropertyRemove xmmsc id (Just src) key =
  liftResult $ B.medialibEntryPropertyRemoveWithSource xmmsc id src key
medialibEntryPropertyRemove xmmsc id Nothing key =
  liftResult $ B.medialibEntryPropertyRemove xmmsc id key

xformMediaBrowse :: Connection -> URL -> IO (Result Default [BrowseEntry])
xformMediaBrowse xmmsc url =
  liftResult $ B.xformMediaBrowse xmmsc url

xformMediaBrowseEncoded
  :: Connection
  -> EncodedURL
  -> IO (Result Default [BrowseEntry])
xformMediaBrowseEncoded xmmsc url =
  liftResult $ B.xformMediaBrowseEncoded xmmsc url


broadcastMedialibEntryChanged :: Connection -> IO (Result Broadcast MediaId)
broadcastMedialibEntryChanged =
  liftResult . B.broadcastMedialibEntryChanged

broadcastMedialibEntryAdded
  :: Connection
  -> IO (Result Broadcast MediaId)
broadcastMedialibEntryAdded =
  liftResult . B.broadcastMedialibEntryAdded
