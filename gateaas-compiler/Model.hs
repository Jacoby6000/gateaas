{-# LANGUAGE DeriveTraversable, TemplateHaskell, TypeFamilies, RecursiveDo #-}
module Model(BoolExpr(..), BoolExprF(..)) where 
  
import Data.Functor.Foldable.TH (makeBaseFunctor)

data BoolExpr
  = BoolProgram [BoolExpr]
  | AND BoolExpr BoolExpr
  | NAND BoolExpr BoolExpr
  | OR BoolExpr BoolExpr
  | NOR BoolExpr BoolExpr
  | XOR BoolExpr BoolExpr
  | XNOR BoolExpr BoolExpr
  | NOT BoolExpr
  -- | LIT Bool
  | INPUT [Char] 
  | OUTPUT [Char] BoolExpr
  | LET [Char] BoolExpr
  | REF [Char]
  deriving(Show)
makeBaseFunctor ''BoolExpr
