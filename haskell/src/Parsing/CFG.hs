module Parsing.CFG where

import Data.Functor.Const
import Data.Functor.Foldable
import Reification (Graph(..), toGraph)
import Parsing.Parsing
import qualified Data.Map as M
import Data.Map ((!))
import System.IO.Unsafe

data CFG s = Eps | Any | Sym s | Nam String (CFG s) | Alt (CFG s) (CFG s) | Seq (CFG s) (CFG s)

data CFGF s a = EpsF | AnyF | SymF s | NamF String a | AltF a a | SeqF a a
    deriving (Functor, Foldable, Traversable)

deriving instance (Show s, Show a) => Show (CFGF s a)

type instance Base (CFG s) = CFGF s

instance Recursive (CFG s) where
    project Eps       = EpsF
    project Any       = AnyF
    project (Sym a)   = SymF a
    project (Nam n s) = NamF n s
    project (Alt a b) = AltF a b
    project (Seq a b) = SeqF a b

instance Corecursive (CFG s) where
    embed EpsF       = Eps
    embed AnyF       = Any
    embed (SymF a)   = Sym a
    embed (NamF n s) = Nam n s
    embed (AltF a b) = Alt a b
    embed (SeqF a b) = Seq a b

newtype CFGGraph s = CFGGraph (Graph (CFGF s))

instance Parsing (Const (CFG s)) s where
    sym  :: s -> Const (CFG s) ()
    sym s = Const $ Sym s

    eps  :: a -> Const (CFG s) a
    eps a = Const $ Eps

    any  :: Const (CFG s) s
    any = Const $ Any

    alt  :: Const (CFG s) a -> Const (CFG s) b -> Const (CFG s) (Either a b)
    alt (Const a) (Const b)             = Const $ Alt a b

    seq  :: Const (CFG s) a -> Const (CFG s) b -> Const (CFG s) (a, b)
    seq (Const a) (Const b)             = Const $ Seq a b

    imap :: Iso a b -> Const (CFG s) a -> Const (CFG s) b
    imap f (Const a) = (Const a)

    rule :: String -> Const (CFG s) a -> Const (CFG s) a
    rule name (Const a) = Const $ Nam name a

isNullableG :: Graph (CFGF s) -> Bool
isNullableG (Graph map) = nullability ! 0 where
    nullability = M.map
        (\case EpsF -> True;
               AnyF -> False
               SymF _ -> False
               NamF _ k -> nullability ! k
               AltF l r -> (nullability ! l) || (nullability ! r)
               SeqF l r -> (nullability ! l) && (nullability ! r)) map

isNullable :: forall a s. (forall p. Parsing p s => p a) -> Bool
isNullable p = unsafePerformIO $ do
  g <- toGraph $ getConst (p :: Const (CFG s) a)
  pure $ isNullableG g