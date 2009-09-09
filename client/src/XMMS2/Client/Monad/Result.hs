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
  , liftXMMSResult
  , result
  , resultRawValue
  , handler
  ) where

import Control.Monad.State
import Data.Maybe
import XMMS2.Client.Monad.Monad
import XMMS2.Client.Monad.Value
import qualified XMMS2.Client.Result as XR
import Data.Int (Int32)
import System.IO.Error  


class ResultType t where
  valueToType :: Value -> XMMS t
                 

instance ResultType () where
  valueToType _ = return ()

instance ResultType Value where
  valueToType = return

instance ResultType Int32 where
  valueToType = getInt

instance ResultType String where
  valueToType = getString

instance ResultType a => ResultType [a] where
  valueToType v = do
    s <- listGetSize v
    mapM (\n -> listGet v n >>= valueToType) [0 .. (s - 1)]


type ResultM a b = StateT (Maybe a, Value) XMMS b

runResultM :: ResultType a => ResultM a b -> Value -> XMMS b
runResultM f v = evalStateT f (Nothing, v)

result :: ResultType a => ResultM a a
result = do
  (res, raw) <- get
  case res of
    Just val ->
      return val
    Nothing  ->
      do val <- lift $ valueToType raw
         put (Just val, raw)
         return val

resultRawValue :: ResultType a => ResultM a Value
resultRawValue = gets snd


data (ResultType a) => Result a = Result XR.Result
                                
handler :: ResultType a => XMMS (Result a) -> ResultM a Bool -> XMMS ()
handler r f = do
  Result r' <- r
  xmmsc <- connection
  liftIO $ XR.resultNotifierSet r' $ runHandler f xmmsc

runHandler f xmmsc v = do
  r <- runXMMS (runResultM f v) xmmsc
  case r of
    Right gr -> return gr
    Left err -> ioError $ userError err
        

liftXMMSResult = liftM Result . liftXMMS                                    
