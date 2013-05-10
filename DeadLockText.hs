import Control.Concurrent.MVar
import Control.Monad
import System.IO

main = do
  -- set up a mutex, filled with topics of art and programming
  artAndProgramming <- newEmptyMVar
  -- give the mutex to Erin
  void $ forkIO $ erin artAndProgramming
  -- give the mutex to Chris
  void $ forkIO $ chris artAndProgramming
  -- forever wait a while some rando puts something in the mutex
  forever $ do
      threadDelay d
      rando artAndProgramming

d = length "who knows how long"

-- in her own little world
erin q = forever $ do
    -- Erin tries to take something from art and programming forever
    -- in deadlock with Chris
    res <- takeMVar q
    -- once she finally gets something from the mvar
    {-
      if it's beer: drink it and iterate immediately
      if it's van gogh: talk senselessly about it for a while, then iterate
      if it's beagleboard: delay for a bit and then lose interest
      if it's any other code: get stuck in an infinite loop
      if it's anything else: cache it and move on
    -}
    case res of
        "beer" -> return ()
        "van gogh" -> hPutStrLn stdout "blah blah blah van gogh"
        "beagleboard" -> threadDelay d
        "code" -> forever $ threadDelay d
        _ -> cacheValue res

-- in his own little world
chris q = forever $ do
    -- Chris tries to take something from art and programming forever
    -- in deadlock with Erin
    res <- takeMVar q
    -- once he finally gets something from the mvar
    -- anything
    -- he "context switches" by blurting out what it was and iterating
    contextSwitch res
  where
    contextSwitch res = hPutStrLn stdout (show res) >> chris

-- random randos only put beer in art and programming
rando q = forever $ do
    putMVar "beer" q

{-
tl;dr
this could be more appropriately titled "an infinite loop of beer"
-}
