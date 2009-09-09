module Main
  where

import qualified XMMS2.Client.Connection as X
import XMMS2.Client.Monad
import XMMS2.Client.Monad.Result
import XMMS2.Client.Monad.Value

main = do
  (Just c) <- X.init "test2"
  True     <- X.connect c Nothing
  runXMMS (do r <- playlistListEntries Nothing
              resultWait r
              v <- resultGetValue r
              l <- getList getInt v
              mapM_ (\i -> liftIO $ print i) (take 10 l)
          ) c


-- local variables:
-- compile-command: "ghc --make Test3.hs"
-- end:
