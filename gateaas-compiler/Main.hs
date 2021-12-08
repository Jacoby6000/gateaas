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
  _outputs :: [GateRef],
  _name :: Maybe String
}
  deriving(Eq, Show)

data GateRef = GateRef {
  _gateRefType :: GateType,
  _inputRef :: Int,
  _nameRef :: Maybe String
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
appendVarBinding g@(Gate _ _ (Just name)) (Env b i pmin pmax) = Env (g:b) (Set.insert name i) pmin pmax
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
      buildService portOffset (Gate tpe outs name) = intercalate "\n" [
        "  " <> serviceName <> ": ",
        "    image: gateaas:1.0.0",
        "    container_name: gateaas-" <> serviceName,
        "    ports:",
        "      - \"" <> show (portOffset + minPort) <> ":80\"",
        "    environment:",
        "      - PORT=80",
        "      - BIND_ADDRESS=0.0.0.0",
        "      - GATE_TYPE=" <> show tpe,
        "      - OUTPUTS=" <> intercalate "," (gateUrl portOffset <$> outs),
        ""
        ]
          where 
            serviceName = gateName portOffset name tpe



progToInOutMap :: Env -> BoolExpr -> Either String Env
progToInOutMap initialEnv expr = finalResult
  where
    initialResult = cata go expr

    initialBindings :: [Gate]
    initialBindings = _bindings initialResult
    
    usedVars = do
      outs <- _outputs <$> initialBindings
      catMaybes $ _nameRef <$> outs

    existingVars = _vars initialResult

    badVars = Set.fromList usedVars Set.\\ existingVars


    finalResult = 
      if Set.empty /= badVars then
        Left $ "References to undefined variables: " <> show badVars
      else
        Right initialResult

    go :: BoolExprF Env -> Env
    go (BoolProgramF envs) = sconcat ((NonEmpty.:|) initialEnv envs)
    go (ANDF l r) = newBinEnv AND l r
    go (NANDF l r) = newBinEnv NAND l r
    go (ORF l r) = newBinEnv OR l r
    go (NORF l r) = newBinEnv NOR l r
    go (XORF l r) = newBinEnv XOR l r
    go (XNORF l r) = newBinEnv XNOR l r
    go (NOTF e) = newUnaryEnv XNOR Nothing e
    go (REFF name) = newUnaryEnv NOOP (Just name) initialEnv
    go (LETF name e) = newBindingEnv NOOP name e
    go (INPUTF name) = newBindingEnv NOOP name initialEnv
    go (OUTPUTF name e) = newUnaryEnv NOOP (Just name) e

    -- TODO: Make this newFooEnv stuff better.
    newBindingEnv :: GateType -> String -> Env -> Env
    newBindingEnv tpe name e@(Env [] _ _ _) = appendVarBinding (gate tpe (Just name) []) e
    newBindingEnv tpe name e = appendVarBinding newGate e 
      where
        newGate = gate tpe (Just name) [gateRef 0 (latestBinding e)]


    newUnaryEnv :: GateType -> Maybe String -> Env -> Env
    newUnaryEnv tpe name e@(Env [] _ _ _) = appendBinding (gate tpe name []) e
    newUnaryEnv tpe name e = appendBinding newGate e 
      where
        newGate = gate tpe name [gateRef 0 (latestBinding e)]

    newBinEnv :: GateType -> Env -> Env -> Env
    newBinEnv tpe l r = appendBinding newGate updatedEnv
      where
        updatedEnv = l <> r
        newGate = 
          gate tpe Nothing [
            gateRef 0 (latestBinding l),
            gateRef 1 (latestBinding r)
          ]
    

gateName :: Int -> Maybe String -> GateType -> String
gateName idx Nothing tpe = show tpe <> "_" <> show idx
gateName _ (Just name) _ = name

gateUrl :: Int -> GateRef -> String
gateUrl idx (GateRef tpe inputNum name) = "http://" <> gateName idx name tpe <> "/in/" <> show inputNum

gateRef :: Int -> Gate -> GateRef
gateRef idx (Gate tpe _ name) = GateRef tpe idx name

gate :: GateType -> Maybe String -> [GateRef] -> Gate 
gate tpe name refs = Gate tpe refs name

