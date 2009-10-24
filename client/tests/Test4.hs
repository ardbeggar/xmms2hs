module Main
  where

import qualified XMMS2.Client.Connection as X
import XMMS2.Client.Monad
import XMMS2.Client.Monad.Result
import XMMS2.Client.Monad.Value
import XMMS2.Client.Glib  
import System.Glib.MainLoop
import Control.Monad.Trans
  
  
main = do
  mloop <- mainLoopNew Nothing False
  (Just c) <- X.init "test4"
  True     <- X.connect c Nothing
  mainLoopGMainInit c
  runXMMS (getEntries `handler` printEntries mloop) c
  mainLoopRun mloop
             
printEntries mloop = do
  entries <- result
  liftIO $ mapM_ print $ (take 5 entries)
  liftIO $ mapM_ print $ (take 7 entries)
  liftIO $ mainLoopQuit mloop
  return False

getEntries :: XMMS (Result [Int32])
getEntries = playlistListEntries Nothing
              

-- local variables:
-- compile-command: "ghc --make Test4.hs"
-- end:
