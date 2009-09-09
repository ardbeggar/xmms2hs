module Main
  where

import qualified XMMS2.Client.Connection as X
import XMMS2.Client.Result
import XMMS2.Client.Value
import XMMS2.Client.Playlist
import Data.Maybe
import Control.Monad
import System.IO.Unsafe  

main = do
  (Just c) <- X.init "test2"
  True <- X.connect c Nothing
  r <- playlistListEntries c Nothing
  resultWait r
  v <- resultGetValue r
  l <- getList v
  mapM_ print (take 10 l)

getList v = do
  (Just i) <- getListIter v
  getList' i
  where
    getList' i = do
      valid <- listIterValid i
      if valid
         then do (Just val) <- listIterEntry i
                 (Just itm) <- getInt val
                 listIterNext i
                 putStrLn "next..."
                 liftM (itm :) $ unsafeInterleaveIO $ getList' i
         else return []


-- local variables:
-- compile-command: "ghc --make Test2.hs"
-- end:
