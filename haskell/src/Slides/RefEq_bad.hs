module Slides.RefEq_bad where

import System.Mem.StableName
import System.IO.Unsafe

refEq :: a -> a -> Bool
refEq x y = eqStableName (unsafePerformIO $ makeStableName x) (unsafePerformIO $ makeStableName y)

list = 1 : list

fib = 1 : 1 : zipWith (+) fib (tail fib)
-- Anything simpler and GHC will optimize list2 as list.
list2 = if fib !! 7 == 21 then 1 : list2 else []

list3 () = 1 : list3 ()

main' :: IO ()
main' = do
  putStrLn $ "refEq list list   = " <> show (refEq list list)
  putStrLn $ "refEq list2 list2 = " <> show (refEq list2 list2)
  putStrLn $ "refEq list3 list3 = " <> show (refEq (list3 ()) (list3 ()))
  putStrLn $ "refEq list list2  = " <> show (refEq list list2)
  putStrLn $ "refEq ⊥ ⊥         = " <> show (refEq undefined undefined)