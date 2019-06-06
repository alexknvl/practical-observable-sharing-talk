module Parsing.ParseTree where

import Prelude hiding (any, seq)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Functor ((<&>))
import Control.Monad.State.Lazy

import Parsing.Parsing

data ParseTree s = Leaf s | Unnamed (NonEmpty (ParseTree s)) | Named String (NonEmpty (ParseTree s))

instance Semigroup (ParseTree s) where
    Unnamed t1 <> Unnamed t2     = Unnamed (t1 <> t2)
    Unnamed t1 <> t2@(Leaf _)    = Unnamed (t1 <> NE.fromList [t2])
    Unnamed t1 <> t2@(Named _ _) = Unnamed (t1 <> NE.fromList [t2])
    t1@(Leaf _)    <> Unnamed t2 = Unnamed (NE.fromList [t1] <> t2)
    t1@(Named _ _) <> Unnamed t2 = Unnamed (NE.fromList [t1] <> t2)
    t1 <> t2                     = Unnamed (t1 :| [t2])

newtype WithParseTree f s a = WithParseTree {
    runParseTree :: f (a, Maybe (ParseTree s))
}

instance (Parsing f s, Functor f) => Parsing (WithParseTree f s) s where
    sym :: s -> WithParseTree f s ()
    sym s = WithParseTree $ (,Just $ Leaf s) <$> sym s

    eps :: a -> WithParseTree f s a
    eps a = WithParseTree $ (,Nothing) <$> eps a

    any :: WithParseTree f s s
    any = WithParseTree $ (\s -> (s, Just $ Leaf s)) <$> any

    alt pa pb = WithParseTree $ runParseTree pa `alt` runParseTree pb <&>
        \case Left  (x, s) -> (Left x,  s)
              Right (x, s) -> (Right x, s)

    seq pa pb = WithParseTree $ runParseTree pa `seq` runParseTree pb <&>
        \case ((a, t1), (b, t2)) -> ((a, b), t1 <> t2)

    imap (Iso to from) pa = WithParseTree $ imap (Iso (\case (a, t) -> (to a, t)) (\case (b, t) -> (from b, t))) (runParseTree pa)

    rule name pa = WithParseTree $ runParseTree pa <&>
        \case (a, Nothing)            -> (a, Nothing)
              (a, Just t@(Leaf _))    -> (a, Just $ Named name (NE.fromList [t]))
              (a, Just t@(Named _ _)) -> (a, Just $ Named name (NE.fromList [t]))
              (a, Just (Unnamed t))   -> (a, Just $ Named name t)

instance Show s => Show (ParseTree s) where
    show t = "digraph G {\n" <> (snd . fst) (runState (go t) 0) <> "}" where
        next :: State Int Int
        next = do
            i <- get
            put $ i + 1
            pure i

        go :: ParseTree s -> State Int (Int, String)
        go (Leaf s)    = next <&> \i -> (i, "   " <> show i <> " [label=\"" <> show s <> "\"]\n")
        go (Unnamed x) = gosub "?" x
        go (Named n x) = gosub n   x

        gosub :: String -> NonEmpty (ParseTree s) -> State Int (Int, String)
        gosub name children = do
            root <- next
            subnodes <- traverse go children
            let subnodeText :: [Char]
                subnodeText = flip foldMap subnodes $
                                 \case (i, s) -> s <> "   " <> show root <> " -> " <> show i <> "\n"
            pure (root, "   " <> show root <> " [label=\"" <> name <> "\"]\n" <> subnodeText)