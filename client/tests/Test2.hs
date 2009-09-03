module Main
  where

import qualified XMMS2.Client.Connection as X
import XMMS2.Client.Result
import XMMS2.Client.Value
import XMMS2.Client.Playlist
import Data.Maybe  

main = do
  (Just c) <- X.init "test1"
  True <- X.connect c Nothing
  r <- listEntries c Nothing
  wait r
  v <- getValue r
  s <- listGetSize v
  mapM_ (\n -> do { putStr (show n)
                  ; putStr ": "
                  ; (Just i) <- listGet v n
                  ; getInt i >>= print . fromJust
                  }) [0 .. (s - 1)]

-- local variables:
-- compile-command: "ghc --make Test2.hs"
-- end:
