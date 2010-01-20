module Main
  where

import Prelude hiding (init)    
import XMMS2.Client.Connection
import XMMS2.Client.Result
import XMMS2.Client.Value
import XMMS2.Client.Playlist  

main = do
  (Just c) <- init "test3"
  True     <- connect c Nothing
  entries  <- getEntries c
  mapM_ print $ (take 5 entries)
  mapM_ print $ (take 10 entries)

getEntries :: Connection -> IO [Int32]
getEntries c = do
  r <- playlistListEntries c Nothing
  resultWait r
  v <- resultGetValue r
  lazyGetList v
              

-- local variables:
-- compile-command: "ghc --make Test3.hs"
-- end:
