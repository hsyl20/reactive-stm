import Reactive

import Control.Concurrent

main :: IO ()
main = do

   x <- newDyn (0 :: Int)
   y <- newDyn 5
   z <- newDyn 2

   t1 <- readDynIO z
   putStrLn $ "z before assignation: " ++ show t1

   assignDyn z (adder x y)

   -- give some time to the assign thread to be executed
   threadDelay 100

   t2 <- readDynIO z
   putStrLn $ "z after assignation: " ++ show t2

   writeDynIO x 18
   threadDelay 100

   t3 <- readDynIO z
   putStrLn $ "z after x <- 18: " ++ show t3

   destroyDynIO y
   threadDelay 100

   t4 <- readDynIO z
   putStrLn $ "z after y is destroyed: " ++ show t4


adder :: (Num a, Eq a) => DynValue a -> DynValue a -> Closure (a,a) a
adder = withDyn2 (+)
