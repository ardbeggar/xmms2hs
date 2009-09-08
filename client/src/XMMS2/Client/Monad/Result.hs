-- -*-haskell-*-
--  XMMS2 client library.
--
--  Author:  Oleg Belozeorov
--  Created: 8 Sep. 2009
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

module XMMS2.Client.Monad.Result
  ( Result (..)
  , handler
  ) where

import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Value
import qualified XMMS2.Client.Result as XR


data Result a = Result XR.Result (a -> (Value -> XMMS Bool))

handler :: XMMS (Result a) -> a -> XMMS ()
handler r f = do
  Result r' c <- r
  xmmsc <- connection
  liftIO $ XR.notifierSet r' $ \v -> runXMMS ((c f) v) xmmsc

                                    

    