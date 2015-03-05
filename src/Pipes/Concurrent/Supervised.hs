{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pipes.Concurrent.Supervised
    ( newChannel
    , newChannel'
    , send
    , recv
    , seal
    , fromInput
    , toOutput
    , module X
    ) where

import           Control.Concurrent.Supervised as X
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Maybe
import           Pipes
import           Pipes.Concurrent              as X (Buffer (..))
import qualified Pipes.Concurrent              as Pipes

newChannel :: (MonadBaseControl IO m) => Pipes.Buffer a -> SupervisorT s m (Sender s (MaybeT IO) a, Receiver s (MaybeT IO) a)
newChannel buffer = fmap simplify (newChannel' buffer)
    where simplify (output, input, _) = (output, input)

newChannel' :: (MonadBaseControl IO m) => Pipes.Buffer a -> SupervisorT s m (Sender s (MaybeT IO) a, Receiver s (MaybeT IO) a, IO ())
newChannel' buffer = do
    (o, i, s) <- liftBase $ Pipes.spawn' buffer

    let sender message = MaybeT $ do
            sent <- Pipes.atomically $ Pipes.send o message
            return $ if sent then Just ()
                             else Nothing

        reciever = MaybeT $ Pipes.atomically $ Pipes.recv i

        sealer = Pipes.atomically s

    Channel output input <- registerChannel $ Channel sender reciever
    return (output, input, sealer)

send :: (MonadSupervisor m) => Sender s (MaybeT IO) a -> a -> m Bool
send output = liftM (maybe False $ const True) . liftBase . runMaybeT . output

recv :: (MonadSupervisor m) => Receiver s (MaybeT IO) a -> m (Maybe a)
recv input = liftBase $ runMaybeT input

seal :: (MonadSupervisor m) => IO () -> m ()
seal = liftBase

fromInput :: (MonadSupervisor m) => Receiver s (MaybeT IO) b -> Producer' b m ()
fromInput input = go
    where
        go = do
            ma <- lift $ recv input
            case ma of
                Nothing -> return ()
                Just a  -> do
                    yield a
                    go

toOutput :: (MonadSupervisor m) => Sender s (MaybeT IO) a -> Consumer' a m ()
toOutput output = go
  where
    go = do
        a     <- await
        alive <- lift $ send output a
        when alive go
