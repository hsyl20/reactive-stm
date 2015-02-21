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

   assignDyn (adder x y) z

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
   assignDyn (delay 3 (-1) d) d'

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

   ---------------
   putStrLn "Test linear binding:"
   u <- newDyn (2 :: Int)
   v <- newDyn 200
   bindLinear 4 v u

   let readLoop = do
         value <- readDynIO u
         value2 <- readDynIO v
         putStrLn $ "Current values: " ++ show value ++ " ; " ++ show value2
         case (value,value2) of
            (Value k, Value l) | k == l -> putStrLn "Linear animation completed"
            (Destroyed,_) -> putStrLn "Destroyed!"
            (_,Destroyed) -> putStrLn "Destroyed!"
            (Value k, Value l) | k > 20 && l /= 5  -> do
               putStrLn "Changing target value to 5"
               writeDynIO v 5
               readLoop
            _         -> do
               threadDelay 500
               readLoop

   readLoop

adder :: (Num a, Eq a) => DynValue a -> DynValue a -> Closure (a,a) a
adder = withDyn2 (+)
