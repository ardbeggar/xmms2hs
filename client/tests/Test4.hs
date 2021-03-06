module Main
  where

import qualified XMMS2.Client.Connection as X
import XMMS2.Client.Monad
import XMMS2.Client.Monad.XMMS
import XMMS2.Client.Glib
import System.Glib.MainLoop
import Control.Monad.Trans


main = do
  mloop <- mainLoopNew Nothing False
  (Just c) <- X.init "test4"
  True     <- X.connect c Nothing
  mainLoopGMainInit c
  runXMMS (playlistListEntries Nothing >>* printEntries mloop) c ()
  mainLoopRun mloop

printEntries mloop = do
  entries <- result
  liftIO $ mapM_ print $ (take 5 entries)
  liftIO $ mapM_ print $ (take 10 entries)
  liftIO $ mainLoopQuit mloop
  return False


-- local variables:
-- compile-command: "ghc --make Test4.hs"
-- end:
