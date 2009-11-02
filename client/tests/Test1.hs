module Main
  where

import qualified XMMS2.Client.Connection as X
import XMMS2.Client.Result
import XMMS2.Client.Playback
import XMMS2.Client.Medialib
import XMMS2.Client.Value
import Data.Map (Map)
import qualified Data.Map as Map

main = do
  let url = "file:///srv/share/music/Absu/Tara '2001/01 Tara.flac"
  (Just c) <- X.init "test1"
  X.connect c Nothing
  r <- medialibAddEntry c url
  resultWait r
  r <- medialibGetId c url
  resultWait r
  v <- resultGetValue r
  getInt v >>= print
  testMaps


testMap1 k = Map.fromList $ map (\n -> (show n, DataInt32 n)) [k .. k + 10]
testMap2 = Map.fromList $ map (\n -> (show n, testMap1 n)) [0 .. 5]

testMaps = do
  val <- valueNew testMap2
  map :: Dict (Dict ValueData) <- valueGet val
  print $ map == testMap2

-- local variables:
-- compile-command: "ghc -XScopedTypeVariables --make Test1.hs"
-- end:
