module CFG where

import Data.Functor.Foldable
import Reification (Graph(..))

data CFG s = Any | Rng s s | Nam String (CFG s) | Alt [CFG s] | Seq [CFG s]

data CFGF s a = AnyF | RngF s s | NamF String a | AltF [a] | SeqF [a]
    deriving (Functor, Foldable, Traversable)

deriving instance (Show s, Show a) => Show (CFGF s a)

type instance Base (CFG s) = CFGF s

instance Recursive (CFG s) where
    project Any       = AnyF
    project (Rng a b) = RngF a b
    project (Nam n s) = NamF n s
    project (Alt l)   = AltF l
    project (Seq l)   = SeqF l

instance Corecursive (CFG s) where
    embed AnyF       = Any
    embed (RngF a b) = Rng a b
    embed (NamF n s) = Nam n s
    embed (AltF l)   = Alt l
    embed (SeqF l)   = Seq l

newtype CFGGraph s = CFGGraph (Graph (CFGF s))

instance Show s => Show (CFGGraph s) where

