module Main
  where

import qualified XMMS2.Client.Connection as X
import XMMS2.Client.Result
import XMMS2.Client.Playback
import XMMS2.Client.Value
import XMMS2.Client.Glib
import System.Glib.MainLoop

main = do
  ml <- mainLoopNew Nothing False
  (Just c) <- X.init "test1"
  True     <- X.connect c Nothing
  stop c >>= wait
  start c >>= wait
  r <- signalPlaybackPlaytime c
  notifierSet r $ myPlayTime ml
  mainLoopGMainInit c
  mainLoopRun ml

myPlayTime ml v = do
  (Just t) <- getInt v
  print t
  if t > 5000 then (print "done" >> mainLoopQuit ml >> return False) else return True

-- local variables:
-- compile-command: "ghc --make Test1.hs"
-- end:
