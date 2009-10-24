module Main
  where

import qualified XMMS2.Client.Connection as X
import XMMS2.Client.Monad
import XMMS2.Client.Monad.Result
import XMMS2.Client.Monad.Value

main = do
  (Just c) <- X.init "test3"
  True     <- X.connect c Nothing
  entries  <- runXMMS getEntries c
  mapM_ print $ (take 5 entries)
  mapM_ print $ (take 5 entries)

getEntries :: XMMS [Int32]
getEntries = do
  r <- playlistListEntries Nothing
  resultWait r
  v <- resultGetValue r
  lazyGetList v
              

-- local variables:
-- compile-command: "ghc --make Test3.hs"
-- end:
