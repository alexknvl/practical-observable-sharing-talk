module Network where

import Reification
import Data.Functor.Foldable
import qualified Data.Map as M

data Node = Delay Node | Not Node | And Node Node

data NodeF a = DelayF a | NotF a | AndF a a
    deriving (Functor, Foldable, Traversable)

deriving instance Show a => Show (NodeF a)

deriving instance Show (Graph NodeF)

type instance Base Node = NodeF

instance Recursive Node where
    project (Delay a) = DelayF a
    project (Not a)   = NotF a
    project (And a b) = AndF a b

instance Corecursive Node where
    embed (DelayF a) = Delay a
    embed (NotF a)   = Not a
    embed (AndF a b)   = And a b

clock = Not $ Delay clock
network = And clock (Delay clock)

networkMain :: IO ()
networkMain = do
  Graph graph <- toGraph network
  print $ M.toList graph