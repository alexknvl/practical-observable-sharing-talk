module Parsing.SimpleLang where

import Prelude hiding (seq)
import Data.Functor.Const

import Parsing.Parsing
import Parsing.ParseTree
import Parsing.SimpleParser
import Parsing.CFG
import Data.List.NonEmpty (NonEmpty(..))
import Reification

data Expr = Lit (NonEmpty Bool) | Add Expr Expr | Mul Expr Expr
  deriving (Show, Eq)

exprIso :: Iso (Either (NonEmpty Bool) (Either (Expr, Expr) (Expr, Expr))) Expr
exprIso = Iso (\case Left b -> Lit b;
                     Right (Left (a, b)) -> Add a b;
                     Right (Right (a, b)) -> Mul a b)
              (\case Lit b -> Left b;
                     Add a b -> Right (Left (a, b));
                     Mul a b -> Right (Right (a, b)))

digit :: Parsing p Char => p Bool
digit = imap boolIso (sym '0' `alt` sym '1')

drop135 :: Iso (((((), a), ()), b), ()) (a, b)
drop135 = Iso (\(((((), a), ()), b), ()) -> (a, b))
              (\(a, b) -> (((((), a), ()), b), ()))

lit :: Parsing p Char => p (NonEmpty Bool)
lit = rule "lit" $ many1 digit
add :: Parsing p Char => p (Expr, Expr)
add = rule "add" $ imap drop135 (sym '(' `seq` expr `seq` sym '+' `seq` expr `seq` sym ')')
mul :: Parsing p Char => p (Expr, Expr)
mul = rule "mul" $ imap drop135 (sym '(' `seq` expr `seq` sym '*' `seq` expr `seq` sym ')')

expr :: Parsing p Char => p Expr
expr = imap exprIso (lit `alt` (add `alt` mul))

expr1 :: (Functor p, Parsing p Char) => p ()
expr1 = const () <$> (expr1 `alt` eps ())

main' :: IO ()
main' = do
    putStrLn ""
    putStrLn "## Parsing.SimpleLang"
    print $ runSimpleParser expr "((0+1)*1)"

    let p1 :: WithParseTree (Simple Char) Char Expr
        p1 = expr
    print $ (runSimpleParser . runParseTree) p1 "((0+1)*1)"

    let cfg = getConst (expr :: Const (CFG Char) Expr)
    Graph g <- toGraph cfg
    print $ g

    putStrLn $ "isNullable expr = " <> show (isNullable expr)
    -- putStrLn $ "isNullable expr1 = " <> show (isNullable expr1)
    putStrLn $ "isNullable add = " <> show (isNullable add)
    putStrLn $ "isNullable mul = " <> show (isNullable mul)