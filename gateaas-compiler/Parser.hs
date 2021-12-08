{-# LANGUAGE TypeFamilies, RecursiveDo #-}
module Parser(gateaasGrammar, parseString) where

import Control.Applicative
import Data.Char
import Text.Earley
import Data.List.Split

import Model

parseString :: [Char] -> ([(BoolExpr, Int)], Report [Char] [[Char]])
parseString s = 
  allParses (parser gateaasGrammar) $ wordsBy tokenizer s

tokenizer :: Char -> Bool
tokenizer c = isSpace c || c == '(' || c == ')'

gateaasGrammar :: Grammar r (Prod r [Char] [Char] BoolExpr)
gateaasGrammar = mdo 
  program <- rule $ BoolProgram <$> many x0 <?> "prog-list"
  x0 <- rule $ INPUT <$> (namedToken "INPUT" *> identifier) <|> x1 <?> "input"
  x1 <- rule $ OUTPUT
          <$> (namedToken "OUTPUT" *> identifier) 
          <*> (namedToken "=" *> y1) 
          <|> x2 
  x2 <- rule $ LET 
          <$> (namedToken "LET" *> identifier) 
          <*> (namedToken "=" *> y1)
  y1 <- rule $ AND <$> y1 <* namedToken "AND" <*> y2 <|> y2 
  y2 <- rule $ NAND <$> y2 <* namedToken "NAND" <*> y3 <|> y3 
  y3 <- rule $ OR <$> y3 <* namedToken "OR" <*> y4 <|> y4 
  y4 <- rule $ XOR <$> y4 <* namedToken "XOR" <*> y5 <|> y5 
  y5 <- rule $ XNOR <$> y5 <* namedToken "XNOR" <*> y6 <|> y6
  y6 <- rule $ NOR <$> y6 <* namedToken "NOR" <*> y7 <|> y7
  y7 <- rule $ NOT <$> (namedToken "NOT" *> y8) <|> y8
  y8 <- rule $ REF <$> identifier <|> y10
  -- y9 <- rule $ LIT <$> (toLit <$> satisfy isLit) <|> y10
  y10 <- rule $ namedToken "(" *> y1 <* namedToken ")"
  return program
  where
    ident (x:_) = isAlpha x
    ident _     = False
    identifier = satisfy ident <?> "identifier"

    isTrue "1" = True
    isTrue s = (toLower <$> s) == "true" 
    isFalse "0" = True
    isFalse s = (toLower <$> s) == "false"

    -- isLit s = isTrue s || isFalse s
    -- toLit s
    -- | isFalse s = False
    -- | isTrue s  = True
    -- | otherwise = error "impossible"

