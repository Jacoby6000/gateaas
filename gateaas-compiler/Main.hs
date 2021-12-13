{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.List
import Data.Bifunctor
import System.Environment
import Data.Functor.Foldable(cata)

import qualified Data.Map.Strict as Map 

import Model (BoolExpr, BoolExprF(..))
import Parser
import Interpreter.Docker(envToDocker, initEnv)
import Interpreter.Resolver(resolveBoolExpr)


main :: IO ()
main = do
  (inFile:outFile:_) <- getArgs
  programString <- readFile inFile
  let result = parseString programString 
  let mostParsed = fst $ last (fst result)
  let program = first (\es -> "References to variables before they are defined: " <> intercalate "," es) (resolveBoolExpr mostParsed)
  written <- traverse (writeFile outFile) (envToDocker . initEnv 10000 11000 Map.empty True [] <$> program)
  either error (const . putStrLn $ "Successfully wrote to " <> outFile) written

  -- putStrLn $ progToString mostParsed
  -- pPrint result
  -- pPrint mostParsed
  -- putStrLn ""
  -- putStrLn ""
  -- pPrint env





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
    -- go (LITF True) = "1"
    -- go (LITF False) = "0"
    go (INPUTF s) = "INPUT " <> s
    go (OUTPUTF s expr) = "OUTPUT " <> s <> " = " <> expr
    go (LETF s expr) = "LET " <> s <> " = " <> expr
    go (DEFF name params expr) = "DEF " <> name <> " " <> unwords params <> " = " <> expr
    go (INVOKEF name params) = name <> " " <> unwords params 
    go (REFF s) = s

    bracket :: String -> String
    bracket s = "(" <> s <> ")"

