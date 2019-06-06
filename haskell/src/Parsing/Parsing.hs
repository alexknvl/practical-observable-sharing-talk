module Parsing.Parsing where

import qualified Prelude as P
import Prelude (Either(..), Maybe(..), Bool(..), String, Semigroup(..))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

-- Isomorphisms

data Iso a b = Iso (a -> b) (b -> a)

eitherIso :: Iso (Either () a) (Maybe a)
eitherIso = Iso (\case Left () -> Nothing; Right a -> Just a) (\case Nothing -> Left (); Just a -> Right a)

boolIso :: Iso (Either () ()) Bool
boolIso = Iso (\case Left () -> False; Right () -> True) (\case False -> Left (); True -> Right ())

-- listIso :: Iso (Either () (a, [a])) [a]
-- listIso = Iso (\case Left () -> []; Right (h, t) -> h : t) (\case [] -> Left (); h : t -> Right (h, t))

listIso :: Iso (Either () (NonEmpty a)) [a]
listIso = Iso (\case Left ()      -> [];
                     Right (h :| t) -> h : t)
              (\case []    -> Left ();
                     h : t -> Right (h :| t))


nelIso :: Iso (a, [a]) (NonEmpty a)
nelIso = Iso (\(h, t) -> h :| t)
             (\(h :| t) -> (h, t))


-- Parsing

class Parsing p s | p -> s where
    sym  :: s -> p ()
    eps  :: a -> p a
    any  :: p s
    alt  :: p a -> p b -> p (Either a b)
    seq  :: p a -> p b -> p (a, b)
    imap :: Iso a b -> p a -> p b
    rule :: String -> p a -> p a

unit :: Parsing p s => p ()
unit = eps ()

opt :: Parsing p s => p a -> p (Maybe a)
opt p = imap eitherIso (unit `alt` p)

bool :: Parsing p s => p () -> p Bool
bool p = imap boolIso (unit `alt` p)

seql :: Parsing p s => p a -> p () -> p a
seql a b = imap (Iso P.fst (,())) (a `seq` b)

seqr :: Parsing p s => p () -> p a -> p a
seqr a b = imap (Iso P.snd ((),)) (a `seq` b)

-- many :: Parsing p s => p a -> p [a]
-- many p = r where
--     r = imap listIso (unit `alt` (p `seq` r))

many :: Parsing p s => p a -> p [a]
many p = m where
    m = imap listIso (unit `alt` m1)
    m1 = imap nelIso (p `seq` m)

many1 :: Parsing p s => p a -> p (NonEmpty a)
many1 p = m1 where
    m = imap listIso (unit `alt` m1)
    m1 = imap nelIso (p `seq` m)