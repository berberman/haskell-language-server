module Test.Hls.SessionQueue where

import           Control.Concurrent.Async      (withAsync)
import           Control.Concurrent.Extra      (newBarrier, signalBarrier,
                                                waitBarrier)
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TQueue
import           Control.Monad                 (forever, join)
import           Test.Hls

type SessionQueue = TQueue (Session ())

enqueue :: SessionQueue -> Session a -> IO a
enqueue chan s = do
  b <- newBarrier
  atomically $ writeTQueue chan $ s >>= \x -> liftIO (signalBarrier b x)
  waitBarrier b

runWithSession :: PluginDescriptor IdeState -> FilePath -> (SessionQueue -> IO ()) -> IO ()
runWithSession plugin root f = do
  queue <- newTQueueIO
  withAsync (worker queue) $ \_ ->
    f queue
  where
    worker chan = runSessionWithServer plugin root $ do
      forever $
        join $ liftIO $ atomically $ readTQueue chan
