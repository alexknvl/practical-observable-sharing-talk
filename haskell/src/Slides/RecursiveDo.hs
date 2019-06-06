{-# LANGUAGE RecursiveDo, StandaloneDeriving, DeriveFunctor, DeriveTraversable, TypeFamilies,
             FlexibleContexts #-}

module Slides.RecursiveDo where

import Prelude hiding (and)
import Control.Monad.State.Lazy
import Data.Map as M
import Data.Set as S
import Data.Functor.Foldable

data Signal a = Latch (Signal a) a
              | Xor (Signal a) (Signal a) a
              | And (Signal a) (Signal a) a
              | Inv (Signal a) a

deriving instance Functor Signal
deriving instance Show a => Show (Signal a)

identify :: Signal a -> a
identify (Latch _ a) = a
identify (Xor _ _ a) = a
identify (And _ _ a) = a
identify (Inv _ a)   = a

latch :: Signal Integer -> State Integer (Signal Integer)
latch s = do
    x <- get
    put (x + 1)
    return $ Latch s x

xor  :: Signal Integer -> Signal Integer -> State Integer (Signal Integer)
xor a b = do
    x <- get
    put (x + 1)
    return $ Xor a b x

and  :: Signal Integer -> Signal Integer -> State Integer (Signal Integer)
and a b = do
    x <- get
    put (x + 1)
    return $ And a b x

inv  :: Signal Integer -> State Integer (Signal Integer)
inv a = do
    x <- get
    put (x + 1)
    return $ Inv a x

clock = do
    rec
        l <- latch c
        c <- inv l
    return c

wire1 = do
    rec
        c <- clock
        lw0 <- latch wire0
        wire0 <- xor c lw0
        wire1 <- and c lw0
    return wire1

data SignalF a b = LatchF b a
                 | XorF b b a
                 | AndF b b a
                 | InvF b a

deriving instance Functor     (SignalF a)
deriving instance Foldable    (SignalF a)
deriving instance Traversable (SignalF a)
deriving instance (Show a, Show b) => Show (SignalF a b)

type instance Base (Signal a) = SignalF a

instance Recursive (Signal a) where
    project (Latch x y) = LatchF x y
    project (Xor x y z) = XorF x y z
    project (And x y z) = AndF x y z
    project (Inv x y)   = InvF x y

build :: State Integer (Signal Integer) -> Signal Integer
build s = fst $ runState s 0

toGraph :: Signal Integer -> Map Integer (SignalF Integer Integer)
toGraph node = (snd . flip runState M.empty) (go node) where
    go :: Signal Integer -> State (Map Integer (SignalF Integer Integer)) Integer
    go n = do
        let i = identify n
        seen <- get
        case M.lookup i seen of
            Nothing -> do
                rec
                    put $ M.insert i entry seen
                    entry <- traverse go (project n)
                pure i
            Just _ -> pure i

main' :: IO ()
main' = do
    putStrLn $ "build network = " <> show (toGraph $ build wire1)