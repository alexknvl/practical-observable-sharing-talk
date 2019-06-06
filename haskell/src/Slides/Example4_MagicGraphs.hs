module Slides.Example4_MagicGraphs where

import Reification
import Data.Functor.Foldable
import qualified Data.Map as M
import System.IO.Unsafe

newtype Node = Node [Node]

data NodeF a = NodeF [a]
    deriving (Functor, Foldable, Traversable)

deriving instance Show a => Show (NodeF a)
deriving instance Show (Graph NodeF)

type instance Base Node = NodeF

instance Recursive Node where
    project (Node a) = NodeF a

instance Corecursive Node where
    embed (NodeF a) = Node a

magic :: Node -> Graph NodeF
magic = unsafePerformIO . toGraph

test :: Graph NodeF
test = magic a where
    a = Node [b]
    b = Node [c]
    c = Node [a]

main' :: IO ()
main' = do
    putStrLn ""
    putStrLn "## Magic Graphs"
    print $ test