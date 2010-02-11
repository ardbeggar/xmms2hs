module Main
  where

import Prelude hiding (init)

import XMMS2.Client.Connection
import XMMS2.Client.Result
import XMMS2.Client.Value
import XMMS2.Client.Playlist
import XMMS2.Client.Coll


main = do
  (Just c) <- init "test5"
  True     <- connect c Nothing
  entries  <- getEntries c
  idlist   <- collNew TypeIdlist
  mapM_ (collIdlistAppend idlist) entries
  r <- playlistAddCollection c Nothing idlist []
  resultWait r
  return ()

getEntries :: Connection -> IO [Int32]
getEntries c = do
  r <- playlistListEntries c Nothing
  resultWait r
  v <- resultGetValue r
  getList v


-- local variables:
-- compile-command: "ghc --make Test5.hs"
-- end:
