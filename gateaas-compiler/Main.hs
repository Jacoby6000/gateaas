{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Functor.Foldable (cata)
import Data.Semigroup
import Data.Maybe
import Data.List

import Model (BoolExpr, BoolExprF(..))
import Parser

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

main :: IO ()
main = do
  putStrLn $ progToString mostParsed
  putStrLn ""
  putStrLn ""
  putStrLn $ either error id dockerServices
  where
    result = parseString
      "INPUT a \
      \INPUT b \
      \INPUT cIn \
      \LET aXORb = a XOR b \
      \OUTPUT S = aXORb XOR cIn \
      \OUTPUT cOut = (a AND b) OR (aXORb AND cIn)"
  
    mostParsed = fst $ last (fst result)
    dockerServices = 
      do 
        outMap <- progToInOutMap (Env [] Set.empty 8086 10000) mostParsed
        return $ envToDocker outMap


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
  deriving(Eq, Show)

data Gate = Gate {
  _gateType :: GateType,
  _name :: Maybe String,
  _outputs :: [Gate]
}
  deriving(Eq, Show)

data Env = Env {
  _bindings :: [Gate],
  _vars :: Set.Set String,
  _portMin :: Int,
  _portMax :: Int
}
  deriving(Eq, Show)

instance Semigroup Env where
  (<>) (Env g i pmin pmax) (Env g2 i2 _ _) = Env (g <> g2) (i <> i2) pmin pmax

appendBinding :: Gate -> Env -> Env
appendBinding g (Env b i pmin pmax) = Env (g:b) i pmin pmax

appendVarBinding :: Gate -> Env -> Env
appendVarBinding g@(Gate _ (Just name) _) (Env b i pmin pmax) = Env (g:b) (Set.insert name i) pmin pmax
appendVarBinding g (Env b i pmin pmax) = Env (g:b) i pmin pmax

latestBinding :: Env -> Gate
latestBinding (Env (b:_) _ _ _) = b
latestBinding _ = error "impossible"

envToDocker :: Env -> String
envToDocker (Env gates _ minPort maxPort) =
  "version: '2'\n\
  \services: \n" <> mconcat services
    where 
      services :: [String] 
      services = snd $ foldl (\(p,ss) g -> (p + 1, buildService p g:ss)) (0, []) gates
  
      buildService :: Int -> Gate -> String
      buildService portOffset (Gate tpe name outs) = intercalate "\n" [
        "  " <> serviceName <> ": ",
        "    image: gateaas:1.0.0",
        "    container_name: gateaas-" <> serviceName,
        "    ports:",
        "      - \"" <> show (portOffset + minPort) <> ":80\"",
        "    environment:",
        "      - PORT=80",
        "      - BIND_ADDRESS=0.0.0.0",
        "      - GATE_TYPE=" <> show tpe,
        "      - OUTPUTS=" <> intercalate "," (uncurry (gateUrl portOffset) <$> zip outs [0..]),
        ""
        ]
          where 
            serviceName = gateName portOffset name tpe


declarationNames :: Env -> [String]
declarationNames (Env gs _ _ _) = catMaybes $ go gs
  where
    go :: [Gate] -> [Maybe String]
    go gates@(_:_) = go (_outputs =<< gates) <> (_name <$> gates)
    go [] = []



progToInOutMap :: Env -> BoolExpr -> Either String Env
progToInOutMap initialEnv expr = finalResult
  where
    initialResult = cata go expr []
    
    usedVars = declarationNames initialResult

    existingVars = _vars initialResult

    badVars = Set.fromList usedVars Set.\\ existingVars


    finalResult = 
      if Set.empty /= badVars then
        Left $ "References to undefined variables: " <> show badVars
      else
        Right initialResult

    go :: BoolExprF ([Gate] -> Env) -> ([Gate] -> Env)
    go (BoolProgramF envs) = combineEnvs envs
    go (ANDF l r) = newBinaryEnv AND l r
    go (NANDF l r) = newBinaryEnv NAND l r
    go (ORF l r) = newBinaryEnv OR l r
    go (NORF l r) = newBinaryEnv NOR l r
    go (XORF l r) = newBinaryEnv XOR l r
    go (XNORF l r) = newBinaryEnv XNOR l r
    go (NOTF e) = newUnaryEnv XNOR Nothing e
    go (REFF name) = newUnaryEnv NOOP (Just name) (const initialEnv)
    go (LETF name e) = newBindingEnv NOOP name e
    go (INPUTF name) = newBindingEnv NOOP name (const initialEnv)
    go (OUTPUTF name e) = newBindingEnv NOOP name e
    
    combineEnvs :: [[Gate] -> Env] -> ([Gate] -> Env)
    combineEnvs funcs = \gates ->  sconcat (initialEnv NonEmpty.:| (($gates) <$> funcs))


    newBindingEnv :: GateType -> String -> ([Gate] -> Env) -> ([Gate] -> Env)
    newBindingEnv tpe name gateToEnv = \gates ->
      do
        let newGate = Gate tpe (Just name) gates
        appendVarBinding newGate (gateToEnv [newGate])
    
    newUnaryEnv :: GateType -> Maybe String -> ([Gate] -> Env) -> ([Gate] -> Env)
    newUnaryEnv tpe name gateToEnv = \gates ->
      do
        let newGate = Gate tpe name gates
        appendBinding newGate (gateToEnv [newGate])

    newBinaryEnv :: GateType -> ([Gate] -> Env) -> ([Gate] -> Env) -> ([Gate] -> Env)
    newBinaryEnv tpe gateToEnvL gateToEnvR = \gates ->
      do
        let newGate = Gate tpe Nothing gates
        appendBinding newGate (gateToEnvL [newGate] <> gateToEnvR [newGate])

type GateBuilder = Maybe Gate -> Gate

gateName :: Int -> Maybe String -> GateType -> String
gateName idx Nothing tpe = show tpe <> "_" <> show idx
gateName _ (Just name) _ = name

gateUrl :: Int -> Gate -> Int -> String
gateUrl idx (Gate tpe name _) inputNum = "http://" <> gateName idx name tpe <> "/in/" <> show inputNum

