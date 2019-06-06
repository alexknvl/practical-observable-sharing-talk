module Parsing.SimpleParser where

import Parsing.Parsing (Parsing(..), Iso(..))

newtype Simple s a = Simple {
    runSimpleParser :: [s] -> [([s], a)]
}

deriving instance Functor (Simple s)

instance Eq s => Parsing (Simple s) s where
    sym s = Simple (\case []             -> [];
                          h : t | h == s -> [(t, ())]
                          _              -> [])

    eps a = Simple (\l -> [(l, a)])

    any = Simple (\case [] -> []; h : t -> [(t, h)])

    alt (Simple pa) (Simple pb) = Simple (\l -> (fmap Left <$> pa l) ++ (fmap Right <$> pb l))

    seq (Simple pa) (Simple pb) =
        Simple $ \r0 -> do
            (r1, a) <- pa r0
            (r2, b) <- pb r1
            pure (r2, (a, b))

    imap (Iso to from) (Simple pa) =
        Simple $ \l -> do
            (r, a) <- pa l
            pure (r, to a)

    rule _ pa = pa