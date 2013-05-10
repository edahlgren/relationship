import Control.Concurrent.MVar
import Control.Monad
import System.IO

main = do
  artAndProgramming <- newEmptyMVar
  void $ forkIO $ erin artAndProgramming
  void $ forkIO $ chris artAndProgramming
  forever $ do
      threadDelay d
      rando q

d = length "who knows how long"

erin q = forever $ do
    res <- takeMVar q
    case res of
        "beer" -> return ()
        "van gogh" -> hPutStrLn stdout "blah blah blah van gogh"
        "beagleboard" -> threadDelay d
        "code" -> forever $ threadDelay d
        _ -> cacheValue res

chris q = forever $ do
    res <- takeMVar q
    contextSwitch
  where
    contextSwitch = hPutStrLn stdout (show res) >> chris

rando q = forever $ do
    putMVar "beer" q
