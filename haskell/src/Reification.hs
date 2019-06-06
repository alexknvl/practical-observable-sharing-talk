module Reification (Graph(..), fromGraph, toGraph) where

import qualified Data.Map as M
import qualified Data.HashMap.Lazy as HM
import System.Mem.StableName
import Control.Monad.State.Lazy (StateT, runStateT, get, put, modify)
import Control.Monad.IO.Class
import Data.Hashable (Hashable, hashWithSalt, hash)
import Data.Functor.Foldable

-- Stable names
newtype Name a = Name (StableName a)

instance Eq (Name a) where
    (Name x) == (Name y) = x == y

instance Hashable (Name a) where
    hashWithSalt salt (Name x) = hashWithSalt salt (hashStableName x)

nameOf :: MonadIO m => a -> m (Name a)
nameOf a = liftIO (Name <$> makeStableName a)

-- Graph representation
newtype Graph f = Graph (M.Map Integer (f Integer))

data State t f = State {
    size  :: Integer,
    names :: HM.HashMap (Name t) Integer,
    graph :: M.Map Integer (f Integer)
}

emptyState :: State t f
emptyState = State 0 HM.empty M.empty

toGraph :: forall t f. (Recursive t, Traversable (Base t)) => t -> IO (Graph (Base t))
toGraph node = Graph . graph . snd <$> runStateT (go node) emptyState where
    go :: t -> StateT (State t (Base t)) IO Integer
    go node = do
        name <- nameOf node
        (State oldSize oldNames oldGraph) <- get

        case HM.lookup name oldNames of
            Just key -> return key
            Nothing  -> do
                modify $ \s -> s {
                    size = oldSize + 1,
                    names = HM.insert name oldSize oldNames
                }
                let layer = project node
                entry <- traverse go layer
                modify $ \s -> s { graph = M.insert oldSize entry (graph s) }
                return oldSize

fromGraph :: Corecursive t => Traversable (Base t) => Graph (Base t) -> t
fromGraph (Graph m) = go 0 where
    go i = case M.lookup i m of
                Just fa -> embed $ fmap go fa

class RecursiveK t f | t -> f where
    projectK :: forall a. t a -> f t a
class CorecursiveK t f | t -> f where
    embedK :: forall a. f t a -> t a

newtype Index a = Index Integer
data GraphK f x = GraphK (Index x) (forall a. Index a -> f Index a)

