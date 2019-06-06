module Slides.Example2_Signal where

data Signal = Latch Signal
    | Xor Signal Signal
    | And Signal Signal
    | Inv Signal

clock = Inv (Latch clock)
halfClock = let wire0 = Xor clock (Latch wire0)
                wire1 = And clock (Latch wire0)
            in wire1

simulate :: Signal -> [Bool]
simulate (Latch s) = False : (simulate s)
simulate (Xor a b) = zipWith (/=) (simulate a) (simulate b)
simulate (And a b) = zipWith (&&) (simulate a) (simulate b)
simulate (Inv a)   = map not (simulate a)

main' :: IO ()
main' = do
    putStrLn $ "take 10 $ simulate clock     = " <> show (take 10 $ simulate clock)
    putStrLn $ "take 10 $ simulate halfClock = " <> show (take 10 $ simulate halfClock)