module Slides.Example1_CFG where

import Data.List (stripPrefix)
import Control.Applicative ((<|>))

data CFG = Str String | Seq [CFG] | Alt [CFG]

digit  = Alt [Str "0", Str "1"]
number = Alt [Seq [], Seq [digit, number]]

parse :: CFG -> String -> Maybe String
parse (Str p) s       = stripPrefix p s
parse (Alt []) s      = Nothing
parse (Alt (h : t)) s = parse h s <|> parse (Alt t) s
parse (Seq []) ""     = Just ""
parse (Seq []) _      = Nothing
parse (Seq (h : t)) s = do
    r <- parse h s
    parse (Seq t) r

main' :: IO ()
main' = putStrLn $ "parse number 101 = " <> show (parse number "101")