-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 3 Sep. 2009
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

module XMMS2.Utils
  ( module C2HS
  , withCString
  , withMaybeCString
  , withCStringArray0
  , peekCString
  , withZTArray
  , while
  , lazyWhile
  , takePtr
  , takePtr_
  ) where

import Control.Applicative
import Control.Monad

import System.IO.Unsafe

import Foreign.Ptr
import qualified Foreign.C.String as CS

import Codec.Binary.UTF8.String

import C2HS hiding (withCString, peekCString)


withMaybeCString (Just s) f = withCString s f
withMaybeCString Nothing f  = f nullPtr

withCString = CS.withCString . encodeString

withCStringArray0 [] f =
  f nullPtr
withCStringArray0 sl f =
  allocaArray0 (length sl) (\p -> doIt sl p (f p))
  where
    doIt [] p f =
      poke p nullPtr >> f
    doIt (x:xs) p f =
      withCString x $ \s -> poke p s >> doIt xs (advancePtr p 1) f


peekCString = liftM decodeString . CS.peekCString

while = while' id
lazyWhile = while' unsafeInterleaveIO

while' w c a = w $ do
  continue <- c
  if continue
     then (:) <$> a <*> while' w c a
     else return []


withZTArray = withArray0 0


takePtr  con fin = liftM con . newForeignPtr fin
takePtr_ con     = liftM con . newForeignPtr_