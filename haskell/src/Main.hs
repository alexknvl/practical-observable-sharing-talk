{-# LANGUAGE
  ExistentialQuantification
  , ConstraintKinds
  , KindSignatures
  , RankNTypes
  , ScopedTypeVariables
  , DeriveFunctor
  , DeriveTraversable
  , BangPatterns
  , FunctionalDependencies
  , InstanceSigs
  , StandaloneDeriving
  , UnicodeSyntax
  , ImpredicativeTypes
  , FlexibleInstances
  , FlexibleContexts
  , ScopedTypeVariables
#-}

module Main where

import qualified Data.Map as M
import qualified Data.HashMap.Lazy as HM
import System.Mem.StableName
import Control.Monad.State.Strict (StateT, runStateT, get, put, modify)
import Control.Monad.IO.Class
import Data.Hashable (Hashable, hashWithSalt, hash)

import Tracer

class Functor f => Recursive t f | t -> f where
  embed   :: f t -> t
  project :: t -> f t

  cata :: ∀ z. (f z -> z) -> t -> z
  cata phi = go where
    go = phi . fmap go . project

  -- project = cata (fmap embed)

newtype Graph f = Graph (M.Map Integer (f Integer))

newtype Name a = Name (StableName a)

instance Eq (Name a) where
  (Name x) == (Name y) = x == y

instance Hashable (Name a) where
  hashWithSalt salt (Name x) = hashWithSalt salt (hashStableName x)

data State t f = State {
  size  :: Integer,
  names :: HM.HashMap (Name t) Integer,
  graph :: M.Map Integer (f Integer)
}

emptyState :: State t f
emptyState = State 0 HM.empty M.empty

toGraph :: forall t f. (Recursive t f, Traversable f) => t -> IO (Graph f)
toGraph node = Graph . graph . snd <$> runStateT (go node) emptyState where
  go :: t -> StateT (State t f) IO Integer
  go node = do
    name <- liftIO (Name <$> makeStableName node)
    (State oldSize oldNames oldGraph) <- get

    case HM.lookup name oldNames of
      Just key -> return key
      Nothing  -> do
        modify $ \s -> s {
          size = oldSize + 1,
          names = HM.insert name oldSize oldNames
        }

        entry <- traverse go $ project node
        modify $ \s -> s { graph = M.insert oldSize entry (graph s) }
        return oldSize

fromGraph :: (Recursive t f, Traversable f) => Graph f -> t
fromGraph (Graph m) = go 0 where
  go i = case M.lookup i m of
    Just fa -> embed $ fmap go fa

data Node = Delay Node | Not Node

data NodeF a = DelayF a | NotF a
  deriving (Functor, Foldable, Traversable)

deriving instance Show a => Show (NodeF a)

deriving instance Show (Graph NodeF)

instance Recursive Node NodeF where
  embed (DelayF a) = Delay a
  embed (NotF a)   = Not a

  project (Delay a) = DelayF a
  project (Not a)   = NotF a

network = Not $ Delay network

main :: IO ()
main = do
  Graph graph <- toGraph network
  print $ M.toList graph

-- main = do
--   print $ 10 -- g (3, (2, ()))