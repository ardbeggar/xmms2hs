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
  , ResultType (..)
  , ResultM
  , result
  , handler
  ) where

import Control.Monad.Reader  
import Data.Maybe
import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Value
import qualified XMMS2.Client.Value as XV
import qualified XMMS2.Client.Result as XR
import Data.Int (Int32)

class ResultType t where
  valueToType :: Value -> IO (Maybe t)

instance ResultType () where
  valueToType _ = return Nothing

instance ResultType XV.Value where
  valueToType = return . Just

instance ResultType Int32 where
  valueToType = XV.getInt

instance ResultType String where
  valueToType = XV.getString

instance ResultType a => ResultType [a] where
  valueToType v = do
    s <- XV.listGetSize v
    liftM Just $ mapM (\n -> do
                         Just v' <- XV.listGet v n
                         liftM fromJust $ valueToType v'
                      ) [0 .. (s - 1)]

data (ResultType a) => Result a = Result XR.Result
                                
type ResultM a b = ReaderT (IO (Maybe a)) XMMS b

runResultM :: ResultType a => ResultM a b -> XV.Value -> XMMS b
runResultM f v = runReaderT f (valueToType v)

result :: ResultType a => ResultM a a
result = do
  f <- ask
  Just d <- liftIO $ f
  return d

handler :: ResultType a => XMMS (Result a) -> ResultM a Bool -> XMMS ()
handler r f = do
  Result r' <- r
  xmmsc <- connection
  liftIO $ XR.notifierSet r' $ \v -> runXMMS (runResultM f v) xmmsc
        

                                    

