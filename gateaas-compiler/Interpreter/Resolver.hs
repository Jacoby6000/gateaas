module Interpreter.Resolver(Program(..), Gate(..), GateRef(..), resolveBoolExpr) where

import Data.Functor.Foldable (cata)
import Model(BoolExpr, BoolExprF(..))
import Data.Maybe

import qualified Data.Map.Strict as Map

data GateType
  = AND
  | NAND
  | OR
  | NOR
  | XOR
  | XNOR
  | NOT
  | NOOP
  | REF
  | FUNC_REF 
  | INPUT
  | OUTPUT
  | LET
  deriving(Eq, Show)

data Gate = Gate {
  _gateType :: GateType,
  _name :: String,
  _outputs :: [GateRef]
}
  deriving(Eq, Show)

data GateRef = GateRef {
  _refName :: String,
  _input :: Int
}
  deriving(Eq, Show)


data Program = Program {
  _gates :: [Gate],
  _vars :: Map.Map String Program,
  _func :: Map.Map String ([GateRef] -> Program)
}

instance Semigroup Program where
  (<>) (Program prog vars funcs) (Program prog2 vars2 funcs2) = 
    Program (prog <> prog2) (vars <> vars2) (funcs <> funcs2)

instance Monoid Program where
  mempty = Program [] Map.empty Map.empty

resolveProgram :: Program -> Either [String] Program
resolveProgram (Program gates vars funcs) = do
  let (errs, resolved) = foldl resolve mempty gates
  maybe (Right $ Program (updateGate resolved <$> filter (\g -> _gateType g /= REF) gates) vars funcs) (const $ Left errs) $ head' errs
    where 
      resolve ::([String], Map.Map String Gate) -> Gate -> ([String], Map.Map String Gate)
      resolve (errs, bindings) g@(Gate LET n _) = (errs, Map.insert n g bindings)
      resolve (errs, bindings) g@(Gate INPUT n _) = (errs, Map.insert n g bindings)
      resolve (errs, bindings) (Gate REF n outs) = maybe (n:errs, bindings) (\gate -> (errs, Map.insert n (appendOutputs outs gate) bindings)) $ Map.lookup n bindings
      resolve s _ = s

      updateGate :: Map.Map String Gate -> Gate -> Gate
      updateGate resolved gate = resolvedGate $ fromMaybe gate (Map.lookup (_name gate) resolved)

      resolvedGate :: Gate -> Gate
      resolvedGate (Gate tpe name outputs) = Gate (resolvedGateType tpe) name outputs

      resolvedGateType :: GateType -> GateType
      resolvedGateType LET = NOOP
      resolvedGateType INPUT = NOOP
      resolvedGateType OUTPUT = NOOP
      resolvedGateType t = t

      appendOutputs :: [GateRef] -> Gate -> Gate
      appendOutputs refs (Gate tpe name outputs) = Gate tpe name (outputs <> refs)


resolveBoolExpr :: BoolExpr -> Either [String] Program
resolveBoolExpr expr = finalResult
  where
    finalResult = 
        resolveProgram $ ($[]) <$> fst $ cata go expr
      


    go :: BoolExprF ([GateRef] -> Program, String) -> ([GateRef] -> Program, String)
    go (BoolProgramF progs) = (combinePrograms $ fst <$> progs, "")
    go (ANDF (l, lName) (r, rName)) = newBinaryProgram AND l r (lName <> "-and-" <> rName)
    go (NANDF (l, lName) (r, rName)) = newBinaryProgram NAND l r (lName <> "-nand-" <> rName)
    go (ORF (l, lName) (r, rName)) = newBinaryProgram OR l r (lName <> "-or-" <> rName)
    go (NORF (l, lName) (r, rName)) = newBinaryProgram NOR l r (lName <> "-nor-" <> rName)
    go (XORF (l, lName) (r, rName)) = newBinaryProgram XOR l r (lName <> "-xor-" <> rName)
    go (XNORF (l, lName) (r, rName)) = newBinaryProgram XNOR l r (lName <> "-xnor-" <> rName)
    go (NOTF (e, name)) = newUnaryProgram NOT e ("not-" <> name)
    go (REFF name) = newUnaryProgram REF (const mempty) name
    go (LETF name (e,_)) = newUnaryProgram LET e name
    go (DEFF name params (e, _)) = error "Unsupported"
    go (INPUTF name) = newUnaryProgram INPUT (const mempty ) name
    go (OUTPUTF name (e,_)) = newUnaryProgram OUTPUT e name
    go (INVOKEF name args) = error "Unsupported"
      

    combinePrograms :: [[GateRef] -> Program] -> ([GateRef] -> Program)
    combinePrograms funcs = \gates ->  mconcat (($gates) <$> funcs)

    newUnaryProgram :: GateType -> ([GateRef] -> Program) -> String -> ([GateRef] -> Program, String)
    newUnaryProgram tpe gateToProgram name = 
      (buildNewProgram gateToProgram, name)
        where 
          buildNewProgram l = \refs -> newProgram (Gate tpe name refs) l
          newProgram g l = appendGate g (l [tupleToRef (0,g)])

    newBinaryProgram :: GateType -> ([GateRef] -> Program) -> ([GateRef] -> Program) -> String -> ([GateRef] -> Program, String)
    newBinaryProgram tpe gateToProgramL gateToProgramR name = 
      (buildNewProgram gateToProgramL gateToProgramR, name)
        where 
          buildNewProgram l r = \refs -> newProgram (Gate tpe name refs) l r
          newProgram g l r = appendGate g (l [tupleToRef (0,g)] <> r [tupleToRef (1,g)])

    appendGate :: Gate -> Program -> Program
    appendGate g (Program gates vars refs) = Program (g:gates) vars refs

tupleToRef :: (Int, Gate) -> GateRef
tupleToRef (n, g) = GateRef (_name g) n

head' :: [a] -> Maybe a
head' (a:_) = Just a
head' [] = Nothing
