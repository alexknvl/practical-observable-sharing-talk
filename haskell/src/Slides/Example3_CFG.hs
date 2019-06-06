{-# LANGUAGE GADTs #-}

module Slides.Example3_CFG where

import Data.Functor.Const
import Data.Functor.Foldable
import Data.List (stripPrefix)
import Control.Applicative ((<|>))

data P a where
  Map :: (a -> b) -> P a -> P b
  Str :: a -> String -> P a
  Eps :: a -> P a
  Seq :: (a -> b -> z) -> P a -> P b -> P z
  Alt :: P a -> P a -> P a

digit :: P Integer
digit  = Alt (Str 0 "0") (Str 1 "1")
digits :: P [Integer]
digits = Alt (Eps []) (Seq (:) digit digits)
number :: P Integer
number = Map (foldl (\acc -> \x -> acc * 10 + x) 0) $ digits

parse :: P a -> String -> Maybe (String, a)
parse (Str v p) s      = (,v) <$> stripPrefix p s
parse (Map f p) s      = (\(s, a) -> (s, f a)) <$> parse p s
parse (Eps v) ""       = Just ("", v)
parse (Eps v) s        = Nothing
parse (Seq f pa pb) r0 = do
  (r1, a) <- parse pa r0
  (r2, b) <- parse pb r1
  return (r2, f a b)
parse (Alt pa pb) r0 = parse pa r0 <|> parse pb r0

data PF f a where
  MapF :: (a -> b) -> f a -> PF f b
  StrF :: a -> String -> PF f a
  EpsF :: a -> PF f a
  SeqF :: (a -> b -> z) -> f a -> f b -> PF f z
  AltF :: f a -> f a -> PF f a

deriving instance Functor f => Functor (PF f)

main' :: IO ()
main' = do
    putStrLn $ "parse number 101 = " <> show (parse number "101")