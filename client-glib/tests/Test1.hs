module Main
  where

import qualified XMMS2.Client.Connection as X
import XMMS2.Client.Result
import XMMS2.Client.Playback
import XMMS2.Client.Glib

main = do
  (Just c) <- X.init "test1"
  r <- X.connect c Nothing
  start c >>= wait
  print r

-- local variables:
-- compile-command: "ghc --make Test1.hs"
-- end:
