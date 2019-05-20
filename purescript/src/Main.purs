module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Control.Monad.State.Trans
import Effect.Class (liftEffect)

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
  cata :: forall z. (f z -> z) -> t -> z

main :: Effect Unit
main = do
  log "Hello sailor!"

newtype Graph f = Graph (HM.HashMap Int (f Int))

data State t f = State {
  size  :: Int,
  names :: HM.HashMap (Name t) Int,
  graph :: HM.HashMap Int (f Int)
}

emptyState :: forall t f. State t f
emptyState = State { size: 0, names: HM.empty, graph: HM.empty }

toGraph :: forall t f. Recursive t f => Traversable f => t -> Effect (Graph f)
toGraph node = Graph <$> (\x -> x.graph) <$> snd <$> runStateT (go node) emptyState where
  go :: Recursive t f => Traversable f => t -> StateT (State t f) Effect Int
  go node = do
    name <- liftEffect (Name <$> makeStableName node)
    oldState <- get

    case HM.lookup name oldState.names of
      Just key -> pure key
      Nothing  -> do
        modify $ \s -> s {
          size = oldState.size + 1,
          names = HM.insert name oldState.size oldState.names
        }

        entry <- traverse go $ project node
        modify $ \s -> s { graph = HM.insert oldState.size entry s.graph }
        pure oldState.size

fromGraph :: Recursive t f => Traversable f => Graph f -> t
fromGraph (Graph m) = go 0 where
  go i = case HM.lookup i m of
    Just fa -> embed <$> go fa

data Node = Delay Node | Not Node

data NodeF a = DelayF a | NotF a

-- derive instance Functor
-- derive instance Foldable, Traversable)