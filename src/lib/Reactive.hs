{-# LANGUAGE LambdaCase #-}
module Reactive
   ( Value(..)
   , DynValue (..)
   , Closure
   , newDyn
   , assignDyn
   , readDyn
   , readDynIO
   , writeDynSTM
   , writeDynIO
   , destroyDynSTM
   , destroyDynIO
   , withDyn
   , checkStoreAndReturn
   , storeAndReturn
   , withDyn2
   , withDyn3
   )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
import Control.Applicative ((<$>))

data Value a
   = Value a
   | Destroyed
   deriving (Show)

data DynValue a = DynValue (TVar (Value a))

type Closure s a = StateT (Maybe s) STM (Value a)

newDyn :: a -> IO (DynValue a)
newDyn a = DynValue <$> newTVarIO (Value a)

-- | Assign a closure a to dynamic variable
assignDyn :: DynValue a -> Closure s a -> IO ()
assignDyn (DynValue var) f = void (forkIO (exec Nothing))
   where 
      exec initState = do
         (val,newState) <- atomically $ do
            (v,s) <- runStateT f initState
            writeTVar var v
            return (v,s)
         case val of
            Destroyed -> return ()
            _         -> exec newState


readDynIO :: DynValue a -> IO (Value a)
readDynIO (DynValue a) = readTVarIO a

writeDynSTM :: DynValue a -> a -> STM ()
writeDynSTM (DynValue a) v = writeTVar a (Value v)

writeDynIO :: DynValue a -> a -> IO ()
writeDynIO a v = atomically $ writeDynSTM a v

destroyDynSTM :: DynValue a -> STM ()
destroyDynSTM (DynValue a) = writeTVar a Destroyed

destroyDynIO :: DynValue a -> IO ()
destroyDynIO = atomically . destroyDynSTM

readDyn :: DynValue a -> Closure s a
readDyn (DynValue v) = lift $ readTVar v

withDyn :: DynValue a -> (a -> Closure s b) -> Closure s b
withDyn dyn f = do
   v <- readDyn dyn
   case v of
      Destroyed -> return Destroyed
      Value x   -> f x

storeAndReturn :: s -> b -> Closure s b
storeAndReturn new ret = do
   put (Just new)
   return (Value ret)

checkStoreAndReturn :: Eq s => s -> b -> Closure s b
checkStoreAndReturn new ret = do
   -- check that new state is different
   get >>= \case
      Just old | old == new ->lift $ retry
      _ -> storeAndReturn new ret


withDyn2 :: (Eq a, Eq b) => (a -> b -> r) -> DynValue a -> DynValue b -> Closure (a,b) r
withDyn2 f a b =
   withDyn a $ \a' ->
      withDyn b $ \b' ->
         checkStoreAndReturn (a',b') (f a' b')

withDyn3 :: (Eq a, Eq b, Eq c) => (a -> b -> c -> r) -> DynValue a -> DynValue b -> DynValue c -> Closure (a,b,c) r
withDyn3 f a b c =
   withDyn a $ \a' ->
      withDyn b $ \b' ->
         withDyn c $ \c' ->
            checkStoreAndReturn (a',b',c') (f a' b' c')


-- | Delay value changes
--
-- When the source is destroyed, the target is destroyed too (without any
-- delay)
--delay :: Int -> a -> DynValue a -> Closure ([a],[a])
--delay n def v =
--   withDyn v $ \v' -> do
      
