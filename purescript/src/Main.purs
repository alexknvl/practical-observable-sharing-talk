module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Control.Monad.State.Trans
import Effect.Class (liftEffect)

import Data.Lazy (Lazy, force, defer)
import Control.Lazy (fix)
import Data.Maybe
import Data.Tuple
import Data.Traversable
import Data.Hashable
import Data.HashMap as HM
import Unsafe.StableName (StableName, makeStableName)

newtype Name a = Name (StableName a)

instance nameEq :: Eq (Name a) where
  eq (Name x) (Name y) = x == y

instance nameHashable :: Hashable (Name a) where
  hash (Name x) = hash x

class Functor f <= Recursive t f | t -> f where
  embed   :: f t -> t
  project :: t -> f t

cata :: forall f t z. Recursive t f => (f z -> z) -> t -> z
cata phi = let go x = phi (go <$> project x) in go

newtype Graph f = Graph (HM.HashMap Int (f Int))

type State t f = {
  size  :: Int,
  names :: HM.HashMap (Name t) Int,
  graph :: HM.HashMap Int (f Int)
}

emptyState :: forall t f. State t f
emptyState = { size: 0, names: HM.empty, graph: HM.empty }

toGraph :: forall t f. Recursive t f => Traversable f => t -> Effect (Graph f)
toGraph node = Graph <$> (\x -> x.graph) <$> snd <$> runStateT (toGraph' node) emptyState

toGraph' :: forall t f. Recursive t f => Traversable f => t -> StateT (State t f) Effect Int
toGraph' node = do
  name <- liftEffect (Name <$> makeStableName node)
  oldState <- get

  case HM.lookup name oldState.names of
    Just key -> pure key
    Nothing  -> do
      _ <- modify $ \s -> s {
        size = oldState.size + 1,
        names = HM.insert name oldState.size oldState.names
      }

      entry <- traverse toGraph' $ project node
      _ <- modify $ \s -> s { graph = HM.insert oldState.size entry s.graph }
      pure oldState.size

fromGraph :: forall t f. Partial => Recursive t f => Traversable f => Graph f -> t
fromGraph g = fromGraph' g 0

fromGraph' :: forall t f. Partial => Recursive t f => Traversable f => Graph f -> Int -> t
fromGraph' (Graph m) i = case HM.lookup i m of
  Just fa -> embed (fromGraph' (Graph m) <$> fa)

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
  embed (DelayF a) = Delay (defer $ \_ -> a)
  embed (NotF a)   = Not a

  project (Delay a) = DelayF (force a)
  project (Not a)   = NotF a

network :: Node
network = force $ fix \n -> defer \_ -> Not $ Delay n

main :: Effect Unit
main = do
  (Graph g) <- toGraph network
  log $ show g
