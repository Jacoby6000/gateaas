{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Functor.Foldable (cataA, cata)
import Data.Semigroup
import Data.List
import Data.Maybe
import Data.Bifunctor

import Model (BoolExpr, BoolExprF(..))
import Parser

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map 

main :: IO ()
main = do
  putStrLn $ progToString mostParsed
  putStrLn ""
  putStrLn ""
  let env = first (\e -> "References to variables before they are defined: " <> intercalate "," e) (progToInOutMap (Env [] 10000 11000) mostParsed)
  putStrLn $ either error id (envToDocker <$> env)
  where
    result = parseString
      "INPUT a \
      \INPUT b \
      \INPUT cIn \
      \LET aXORb = a XOR b \
      \OUTPUT S = aXORb XOR cIn \
      \OUTPUT cOut = (a AND b) OR (aXORb AND cIn)"
  
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
    -- go (LITF True) = "1"
    -- go (LITF False) = "0"
    go (INPUTF s) = "INPUT " <> s
    go (OUTPUTF s expr) = "OUTPUT " <> s <> " = " <> expr
    go (LETF s expr) = "LET " <> s <> " = " <> expr
    go (REFF s) = s

bracket :: String -> String
bracket s = "(" <> s <> ")"

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
  | INPUT
  | LET
  | OUTPUT
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

data Env = Env {
  _bindings :: [Gate],
  _portMin :: Int,
  _portMax :: Int
}
  deriving(Eq, Show)

instance Semigroup Env where
  (<>) (Env g pmin pmax) (Env g2 _ _) = Env (g <> g2) pmin pmax

appendBinding :: Gate -> Env -> Env
appendBinding g (Env b pmin pmax) = Env (g:b) pmin pmax

appendOutputs :: [GateRef] -> Gate -> Gate
appendOutputs refs (Gate tpe name outputs) = Gate tpe name (outputs <> refs)

resolvedGate :: Gate -> Gate
resolvedGate (Gate tpe name outputs) = Gate (resolvedGateType tpe) name outputs

resolvedGateType :: GateType -> GateType
resolvedGateType LET = NOOP
resolvedGateType INPUT = NOOP
resolvedGateType OUTPUT = NOOP
resolvedGateType t = t

envToDocker :: Env -> String
envToDocker (Env gates minPort _) =
  "version: '2'\n\
  \services: \n" <> mconcat (reverse services)
    where 
      services :: [String] 
      services = snd $ foldl (\(p,ss) g -> (p + 1, buildService p g:ss)) (0, []) gates
  
      buildService :: Int -> Gate -> String
      buildService portOffset (Gate tpe name outs) = intercalate "\n" [
        "  " <> name <> ": ",
        "    image: gateaas:1.0.0",
        "    container_name: gateaas-" <> name,
        "    ports:",
        "      - \"" <> show (portOffset + minPort) <> ":80\"",
        "    environment:",
        "      - PORT=80",
        "      - BIND_ADDRESS=0.0.0.0",
        "      - GATE_TYPE=" <> show tpe,
        "      - OUTPUTS=" <> intercalate "," (gateUrl <$> outs),
        ""
        ]

resolveEnv :: Env -> Either [String] Env
resolveEnv (Env gates mn mx) = do
  let (errs, resolved) = foldl resolve ([], Map.empty) gates
  maybe (Right $ Env (updateGate resolved <$> filter (\g -> _gateType g /= REF) gates) mn mx) (const $ Left errs) $ head' errs
    where 
      resolve ::([String], Map.Map String Gate) -> Gate -> ([String], Map.Map String Gate)
      resolve (errs, bindings) g@(Gate LET n _) = (errs, Map.insert n g bindings)
      resolve (errs, bindings) g@(Gate INPUT n _) = (errs, Map.insert n g bindings)
      resolve (errs, bindings) (Gate REF n outs) = maybe (n:errs, bindings) (\gate -> (errs, Map.insert n (appendOutputs outs gate) bindings)) $ Map.lookup n bindings
      resolve s _ = s

      updateGate :: Map.Map String Gate -> Gate -> Gate
      updateGate resolved gate = resolvedGate $ fromMaybe gate (Map.lookup (_name gate) resolved)

progToInOutMap :: Env -> BoolExpr -> Either [String] Env
progToInOutMap initialEnv expr = finalResult
  where
    finalResult = 
        resolveEnv $ ($[]) <$> fst $ cataA go expr
        --(consolidateEnv <$> validatedResult)
      


    go :: BoolExprF ([GateRef] -> Env, String) -> ([GateRef] -> Env, String)
    go (BoolProgramF envs) = (combineEnvs $ fst <$> envs, "")
    go (ANDF (l, lName) (r, rName)) = newBinaryEnv AND l r (lName <> "-and-" <> rName)
    go (NANDF (l, lName) (r, rName)) = newBinaryEnv NAND l r (lName <> "-nand-" <> rName)
    go (ORF (l, lName) (r, rName)) = newBinaryEnv OR l r (lName <> "-or-" <> rName)
    go (NORF (l, lName) (r, rName)) = newBinaryEnv NOR l r (lName <> "-nor-" <> rName)
    go (XORF (l, lName) (r, rName)) = newBinaryEnv XOR l r (lName <> "-xor-" <> rName)
    go (XNORF (l, lName) (r, rName)) = newBinaryEnv XNOR l r (lName <> "-xnor-" <> rName)
    go (NOTF (e, name)) = newUnaryEnv NOT e ("not-" <> name)
    go (REFF name) = newUnaryEnv REF (const initialEnv) name
    go (LETF name (e,_)) = newUnaryEnv LET e name
    go (INPUTF name) = newUnaryEnv INPUT (const initialEnv) name
    go (OUTPUTF name (e,_)) = newUnaryEnv OUTPUT e name
    
    combineEnvs :: [[GateRef] -> Env] -> ([GateRef] -> Env)
    combineEnvs funcs = \gates ->  sconcat (initialEnv NonEmpty.:| (($gates) <$> funcs))

    newUnaryEnv :: GateType -> ([GateRef] -> Env) -> String -> ([GateRef] -> Env, String)
    newUnaryEnv tpe gateToEnv name = 
      (buildNewEnv gateToEnv, name)
        where 
          buildNewEnv l = \refs -> newEnv (Gate tpe name refs) l
          newEnv g l = appendBinding g (l [tupleToRef (0,g)])

    newBinaryEnv :: GateType -> ([GateRef] -> Env) -> ([GateRef] -> Env) -> String -> ([GateRef] -> Env, String)
    newBinaryEnv tpe gateToEnvL gateToEnvR name = 
      (buildNewEnv gateToEnvL gateToEnvR, name)
        where 
          buildNewEnv l r = \refs -> newEnv (Gate tpe name refs) l r
          newEnv g l r = appendBinding g (l [tupleToRef (0,g)] <> r [tupleToRef (1,g)])

type GateBuilder = Maybe Gate -> Gate

gateUrl :: GateRef -> String
gateUrl (GateRef name inputNum) = "http://" <> name <> "/in/" <> show inputNum

tupleToRef :: (Int, Gate) -> GateRef
tupleToRef (n, g) = GateRef (_name g) n

head' :: [a] -> Maybe a
head' (a:_) = Just a
head' [] = Nothing
