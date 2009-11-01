module Main
  where

import qualified XMMS2.Client.Connection as X
import XMMS2.Client.Result
import XMMS2.Client.Playback
import XMMS2.Client.Medialib
import XMMS2.Client.Value  

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

-- local variables:
-- compile-command: "ghc --make Test1.hs"
-- end:
