-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 19 Sep. 2009
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

module XMMS2.Client.Monad.Utils
  ( liftGet
  , while
  ) where

import XMMS2.Client.Monad.Monad
import Control.Monad.Error
  

liftGet name f x =
  checkGet ("the value does not hold " ++ name) $ liftIO $ f x

checkGet :: String -> XMMS (Maybe a) -> XMMS a
checkGet text a = do
  a' <- a
  case a' of
    Just r  -> return r
    Nothing -> throwError text
  

while c a = do
  continue <- c
  if continue
    then liftM2 (:) a (while c a)
    else return []
