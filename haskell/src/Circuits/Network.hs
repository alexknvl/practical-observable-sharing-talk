module Circuits.Network where

import Reification
import Data.Functor.Foldable
import qualified Data.Map as M

data Signal = Latch Signal
            | Xor Signal Signal
            | And Signal Signal
            | Inv Signal

data SignalF a = LatchF a
               | XorF a a
               | AndF a a
               | InvF a
    deriving (Functor, Foldable, Traversable)

deriving instance Show a => Show (SignalF a)
deriving instance Show (Graph SignalF)

type instance Base Signal = SignalF

instance Recursive Signal where
    project (Latch a) = LatchF a
    project (Xor a b) = XorF a b
    project (And a b) = AndF a b
    project (Inv a)   = InvF a

instance Corecursive Signal where
    embed (LatchF a) = Latch a
    embed (XorF a b) = Xor a b
    embed (AndF a b) = And a b
    embed (InvF a)   = Inv a

clock = Inv (Latch clock)

adder signal = let lower  = Xor clock (Latch lower)
                   higher = And clock (Latch lower)
                in (higher, lower)

halfClock = fst (adder clock)

network = Xor halfClock (snd $ adder clock)

simulate :: Signal -> [Bool]
simulate (Latch s) = False : simulate s
simulate (Xor a b) = zipWith (/=) (simulate a) (simulate b)
simulate (And a b) = zipWith (&&) (simulate a) (simulate b)
simulate (Inv a)   = map not (simulate a)

main' :: IO ()
main' = do
  putStrLn ""
  putStrLn "## Circuits.Network"
  Graph graph <- toGraph network
  putStrLn $ "toGraph network = " <> show (M.toList graph)
  putStrLn $ "simulated  = " <> show (take 10 $ simulate network)
  putStrLn $ "simulated* = " <> show (take 10 $ simulate $ fromGraph (Graph graph))