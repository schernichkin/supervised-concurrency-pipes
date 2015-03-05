{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Concurrent          (ThreadId, threadDelay)
import           Control.Monad
import           Control.Monad.Base
import           Pipes
import           Pipes.Concurrent.Supervised

consumer :: (MonadIO m) => Consumer Int m ()
consumer = do
    liftIO $ threadDelay 100000
    liftIO $ putStrLn $ "consumer started"
    forever $ do
        a <- await
        liftIO $ do
            putStrLn $ "Got message " ++ (show a)
            threadDelay 200000

producer :: (MonadIO m) => Producer Int m ()
producer = do
    liftIO $ threadDelay 200000
    liftIO $ putStrLn $ "producer started"
    forM_ [0..9] $ \a -> do
        liftIO $ putStrLn $ "Sending message " ++ (show a)
        yield a
        liftIO $ threadDelay 100000

spawnWithTracer :: (MonadSupervisor m) => String -> m () -> m ()
spawnWithTracer name action = do
    threadId <- spawnNamed name action
    void $ spawnNamed ("Tracer for " ++ name) $ tracer threadId Unstarted
    where
        tracer  :: (MonadSupervisor m) => ThreadId -> ThreadState -> m ()
        tracer threadId prevState = do
            maybeInfo <- waitTill $ ThreadState threadId (\info -> if _threadState info /= prevState then Just info else Nothing)
            case maybeInfo of
                Nothing -> return ()
                Just info -> do
                    liftBase $ print info
                    tracer threadId (_threadState info)

main :: IO ()
main = do
    runSupervisorT $ do
        (output, input) <- newChannel Unbounded
        spawnWithTracer "consumer" $ runEffect (fromInput input >-> consumer)
        spawnWithTracer "producer" $ runEffect (producer >-> toOutput output)
        waitTill NoRunningThreads
    putStrLn "All threads terminated"
