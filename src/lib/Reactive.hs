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
   , delay
   , bind
   , bindWith
   , clockTime
   , bindLinear
   )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift,MonadTrans)
import Control.Applicative ((<$>))

import System.Clock
import GHC.Conc.Sync (unsafeIOToSTM)

data Value a
   = Value a
   | Destroyed
   deriving (Show)

data DynValue a = DynValue (TVar (Value a))

type Closure s a = StateT (Maybe s) STM (Value a)

newDyn :: a -> IO (DynValue a)
newDyn a = DynValue <$> newTVarIO (Value a)

-- | Assign a closure a to dynamic variable
assignDyn :: Closure s a -> DynValue a -> IO ()
assignDyn f v = assignDynWithDelay 0 f v

-- | Assign a closure a to dynamic variable with a specific delay
assignDynWithDelay :: Int -> Closure s a -> DynValue a -> IO ()
assignDynWithDelay d f (DynValue var) = do
      -- Set initial value synchronously
      (val,newState) <- exec1 Nothing
      case val of
         Destroyed -> return ()
         _         -> void (forkIO (exec newState))
   where 
      exec1 initState = atomically $ do
         -- We do not update a Destroyed variable
         oldv <- readTVar var
         case oldv of
            Destroyed -> return (oldv, initState)
            _         -> do
               (v,s) <- runStateT f initState
               writeTVar var v
               return (v,s)

      exec initState = do
         (val,newState) <- exec1 initState
         case val of
            Destroyed -> return ()
            _         -> do
               -- Make the spinning thread sleep a little bit if necessary
               if d > 0
                  then threadDelay d
                  else yield
               exec newState


readDynIO :: DynValue a -> IO (Value a)
readDynIO (DynValue a) = readTVarIO a

writeDynSTM :: DynValue a -> a -> STM ()
writeDynSTM (DynValue a) v = do
   old <- readTVar a
   case old of
      Destroyed -> return ()
      _         -> writeTVar a (Value v)

writeDynIO :: DynValue a -> a -> IO ()
writeDynIO a v = atomically $ writeDynSTM a v

destroyDynSTM :: DynValue a -> STM ()
destroyDynSTM (DynValue a) = writeTVar a Destroyed

destroyDynIO :: DynValue a -> IO ()
destroyDynIO = atomically . destroyDynSTM

readDyn :: DynValue a -> Closure s a
readDyn (DynValue v) = lift $ readTVar v

-- | Use a dynamic value. If the value is Destroyed, the closure returns
-- Destroyed too
withDyn :: DynValue a -> (a -> Closure s b) -> Closure s b
withDyn dyn f = do
   v <- readDyn dyn
   case v of
      Destroyed -> return Destroyed
      Value x   -> f x

-- | Store state and return a value
storeAndReturn :: s -> b -> Closure s b
storeAndReturn new ret = do
   put (Just new)
   return (Value ret)

-- | Retry if old state is equal to the new one.
-- Otherwise, call storeAndReturn
checkStoreAndReturn :: Eq s => s -> b -> Closure s b
checkStoreAndReturn new ret = do
   -- check that new state is different
   get >>= \case
      Just old | old == new ->lift $ retry
      _ -> storeAndReturn new ret

withDyn1 :: (Eq a) => (a -> r) -> DynValue a -> Closure a r
withDyn1 f a =
   withDyn a $ \a' ->
      checkStoreAndReturn a' (f a')

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
--
-- If the source changes too fast, some values may be missed (it depends on the
-- thread scheduler).
delay :: Eq a => Int -> a -> DynValue a -> Closure (Int,a,[a],[a]) a
delay n def v =
   withDyn v $ \y -> do
      get >>= \case
         Nothing -> storeAndReturn (n,y,[y],[]) def
         Just st -> case st of
            (_,lst,_,_) | lst == y -> lift $ retry
            (0,_,ys,x:xs) -> storeAndReturn (0,y,y:ys,xs) x
            (0,_,ys,[])   -> let (x:xs) = reverse (y:ys) in storeAndReturn (0,y,[],xs) x
            (m,_,ys,xs)   -> storeAndReturn (m-1,y,y:ys,xs) def
      

binderWith :: Eq a => (a -> r) -> DynValue a -> Closure a r
binderWith f = withDyn1 f

binder :: Eq a => DynValue a -> Closure a a
binder = binderWith id

-- | Bind a value to another
bind :: Eq a => DynValue a -> DynValue a -> IO ()
bind = assignDyn . binder

-- | Bind a value to another with the given modifier function
bindWith :: Eq a => (a -> r) -> DynValue a -> DynValue r -> IO ()
bindWith f = assignDyn . binderWith f

-- | Return monotonic clock time (arbitrary origin). This can be used for
-- animations
clockTime :: MonadTrans t => t STM TimeSpec
clockTime = lift $ unsafeIOToSTM (getTime Monotonic)


binderLinear :: (Eq a, Num a, Ord a, Integral a) => Float -> DynValue a -> DynValue a -> Closure (TimeSpec,Float) a
binderLinear speed src tgt = do
   let
      absSpeed = abs speed * (1e-9 :: Float)

   withDyn src $ \x2 ->
      withDyn tgt $ \x1 -> if x1 == x2
         then lift $ retry
         else do
            t2 <- clockTime
            get >>= \case
               Nothing -> storeAndReturn (t2,0) x1
               Just (t1,dx) -> let
                     tt t = fromIntegral (sec t) * (1e9 :: Float) + (fromIntegral $ nsec t)
                     dt = tt t2 - tt t1
                     dir = if x2-x1 > 0 then 1 else -1
                     realx = fromIntegral x1 + dir * absSpeed * dt + dx
                     x' = ceiling realx
                     x = if dir > 0
                        then min x' x2
                        else max x' x2
                     dx' = realx - fromIntegral x'
                  in storeAndReturn (t2,dx') x

-- | Linear binding
--
-- Speed is in unit/sec
bindLinear :: (Eq a, Num a, Ord a, Integral a) => Float -> DynValue a -> DynValue a -> IO ()
bindLinear speed src tgt = assignDynWithDelay animDelay (binderLinear speed src tgt) tgt

-- | Animation delay (milliseconds)
animDelay :: Int
animDelay = 100
