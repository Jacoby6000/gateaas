{-# LANGUAGE TypeFamilies, RecursiveDo #-}
module Main where

import Data.Functor.Foldable (cata)
import Data.Functor
import Data.Set
import Data.Map.Strict
import Text.Earley
import Text.Earley.Grammar
import Data.Char
import Data.List.Split

import Model
import Parser


data Env a = Env {
  inputs :: Set String,
  bindings :: Map String (BoolExprF a)
}

tokenizer :: Char -> Bool
tokenizer c = isSpace c || c == '(' || c == ')'

main :: IO ()
main = do
  putStrLn $ progToString mostParsed
  where
    result = allParses (parser gateaasGrammar) $ wordsBy tokenizer
      "INPUT a \
      \INPUT b \
      \INPUT cIn \
      \LET aXORb = $a XOR $b \
      \OUTPUT S = $aXORb XOR $cIn \
      \OUTPUT cOut = ($a AND $b) OR ($aXORb AND $cIn)"
  
    mostParsed = fst $ last (fst result)


progToString :: BoolExpr -> String
progToString = cata go
  where
    go :: BoolExprF String -> String
    go (BoolProgramF as) = foldMap (<> "\n") as
    go (ANDF l r) = bracket (l <> " AND " <> r)
    go (NANDF l r) = bracket (l <> " NAND " <> r)
    go (ORF l r) = bracket (l <> " OR " <> r)
    go (NORF l r) = bracket (l <> " NOR " <> r)
    go (XORF l r) = bracket (l <> " XOR " <> r)
    go (XNORF l r) = bracket (l <> " XNOR " <> r)
    go (NOTF n) = bracket ("NOT " <> n)
    go (LITF True) = "1"
    go (LITF False) = "0"
    go (INPUTF s) = "INPUT " <> s
    go (OUTPUTF s expr) = "OUTPUT " <> s <> " = " <> expr
    go (LETF s expr) = "LET " <> s <> " = " <> expr
    go (REFF s) = "$" <> s

bracket :: String -> String
bracket s = "(" <> s <> ")"




