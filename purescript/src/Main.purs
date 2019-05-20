module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Lazy (Lazy, force, defer)
import Control.Lazy (fix)
import Data.Traversable (class Foldable, class Traversable)

import Matryoshka.Class.Recursive (class Recursive)
import Matryoshka.Class.Corecursive (class Corecursive)

import Data.Reified

data Node = Delay (Lazy Node) | Not Node

data NodeF a = DelayF a | NotF a

derive instance nodeFunctor :: Functor NodeF

instance nodeShow :: Show a => Show (NodeF a) where
  show (DelayF a) = "DelayF(" <> show a <> ")"
  show (NotF a)   = "NotF(" <> show a <> ")"

instance nodeFoldable :: Foldable NodeF where
  foldr   :: forall a b. (a -> b -> b) -> b -> NodeF a -> b
  foldr f z (DelayF a) = f a z
  foldr f z (NotF a)   = f a z

  foldl   :: forall a b. (b -> a -> b) -> b -> NodeF a -> b
  foldl f z (DelayF a) = f z a
  foldl f z (NotF a)   = f z a

  foldMap :: forall a m. Monoid m => (a -> m) -> NodeF a -> m
  foldMap f (DelayF a) = f a
  foldMap f (NotF a)   = f a

instance nodeTraversable :: Traversable NodeF where
  traverse :: forall a b m. Applicative m => (a -> m b) -> NodeF a -> m (NodeF b)
  traverse f (DelayF a) = DelayF <$> f a
  traverse f (NotF a)   = NotF   <$> f a

  sequence :: forall a m. Applicative m => NodeF (m a) -> m (NodeF a)
  sequence (DelayF a) = DelayF <$> a
  sequence (NotF a)   = NotF   <$> a

instance nodeRecursive :: Recursive Node NodeF where
  project (Delay a) = DelayF (force a)
  project (Not a)   = NotF a

instance nodeCorecursive :: Corecursive Node NodeF where
  embed (DelayF a) = Delay (defer $ \_ -> a)
  embed (NotF a)   = Not a

network :: Node
network = force $ fix \n -> defer \_ -> Not $ Delay n

main :: Effect Unit
main = do
  (Graph g) <- toGraph network
  log $ show g
