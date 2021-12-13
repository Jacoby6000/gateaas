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
  | INPUT String 
  | OUTPUT String BoolExpr
  | DEF String [String] BoolExpr
  | INVOKE String [BoolExpr]
  | LET String BoolExpr
  | REF String
  deriving(Show)
makeBaseFunctor ''BoolExpr
