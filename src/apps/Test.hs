import Reactive

import Control.Concurrent
import Control.Monad (forM_)

main :: IO ()
main = do

   x <- newDyn (3 :: Int)
   y <- newDyn 5
   z <- newDyn 2

   t1 <- readDynIO z
   putStrLn $ "z before assignation: " ++ show t1

   assignDyn z (adder x y)

   t2 <- readDynIO z
   putStrLn $ "z after assignation: " ++ show t2

   writeDynIO x 18
   -- give some time to the assign thread to be executed
   threadDelay 100

   t3 <- readDynIO z
   putStrLn $ "z after x <- 18: " ++ show t3

   destroyDynIO y
   threadDelay 100

   t4 <- readDynIO z
   putStrLn $ "z after y is destroyed: " ++ show t4


   ---------------
   putStrLn "Test delay:"
   d <- newDyn (17 :: Int)
   d' <- newDyn 0
   assignDyn d' (delay 3 (-1) d)

   forM_ [27..38] $ \i -> do
      writeDynIO d i
      threadDelay 100
      vd <- readDynIO d
      vd' <- readDynIO d'
      putStrLn $ "d: " ++ show vd
      putStrLn $ "delayed d: " ++ show vd'
      if i > 35
         then do
            putStrLn "we destroy delayed d"
            destroyDynIO d
         else return ()



adder :: (Num a, Eq a) => DynValue a -> DynValue a -> Closure (a,a) a
adder = withDyn2 (+)
