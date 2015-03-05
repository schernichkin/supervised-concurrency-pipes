{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Concurrent          (ThreadId)
import           Control.Monad
import           Control.Monad.Base
import           Pipes
import           Pipes.Concurrent.Supervised

data Token = Token String Int deriving ( Show )

thief :: (MonadSupervisor m, MonadIO m) => Pipe Token Token m ()
thief = do
    token@(Token name count)  <- await
    (Just threadName)  <- lift $ getThreadName
    liftIO $ print $ threadName ++ " snitched " ++ (show token)
    when (count > 0) $ yield $ Token name (pred count)
    thief

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
        void $ setThreadName "main"
        (output, input) <- newChannel Unbounded
        forM_ [1..10 :: Int] $ \i -> spawnWithTracer ("thief " ++ (show i))  $ runEffect (fromInput input >-> thief >-> toOutput output)
        forM_ [1..3 :: Int]  $ \i -> send output $ Token ("token " ++ (show i)) $ 6
        waitTill NoRunningThreads
    putStrLn "All threads terminated"
