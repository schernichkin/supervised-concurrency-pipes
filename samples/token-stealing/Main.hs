{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Concurrent          (ThreadId, newMVar, withMVar)
import           Control.Monad
import           Control.Monad.Base
import           Pipes
import           Pipes.Concurrent.Supervised
-- import           Control.Concurrent.Lifted (newMVar, withMVar)

data Token = Token String Int deriving ( Show )

thief :: (MonadSupervisor m, MonadIO m) => (String -> IO ()) -> Pipe Token Token m ()
thief printer = do
    token@(Token name count)  <- await
    (Just threadName)  <- lift $ getThreadName
    liftIO $ printer $ threadName ++ " snitched " ++ (show token)
    when (count > 0) $ yield $ Token name (pred count)
    thief printer 

spawnWithTracer :: (MonadSupervisor m) => String -> (String -> IO ()) -> m () -> m ()
spawnWithTracer name printer action = do
    threadId <- spawnNamed name action
    void $ spawnNamed ("Tracer for " ++ name) $ tracer threadId Unstarted
    where
        tracer  :: (MonadSupervisor m) => ThreadId -> ThreadState -> m ()
        tracer threadId prevState = do
            maybeInfo <- waitTill $ ThreadState threadId (\info -> if _threadState info /= prevState then Just info else Nothing)
            case maybeInfo of
                Nothing -> return ()
                Just info -> do
                    liftBase $ printer (show info)
                    tracer threadId (_threadState info)

main :: IO ()
main = do
    runSupervisorT $ do
        consoleLock <- lift $ newMVar ()
        let printLn = withMVar consoleLock . const . putStrLn
        void $ setThreadName "main"
        (output, input) <- newChannel Unbounded
        forM_ [1..10 :: Int] $ \i -> spawnWithTracer ("thief " ++ (show i)) printLn $ runEffect (fromInput input >-> (thief printLn) >-> toOutput output)
        forM_ [1..3 :: Int]  $ \i -> send output $ Token ("token " ++ (show i)) $ 6
        waitTill NoRunningThreads
    putStrLn "All threads terminated"
