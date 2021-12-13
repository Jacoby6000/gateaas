module Interpreter.Docker(Env(..), initEnv, envToDocker) where

import Data.List

import Interpreter.Resolver(Program(..), Gate(..), GateRef(..))

import qualified Data.Map.Strict as Map

data Env = Env {
  _scope :: Program,
  _portMin :: Int,
  _portMax :: Int,
  _outputMapping :: Map.Map String String,
  _debugMode :: Bool,
  _debugOutputs :: [String]
}

initEnv :: Int -> Int -> Map.Map String String -> Bool -> [String] -> Program -> Env
initEnv minPort maxPort outputMap debugMode debugOuts prog = Env prog minPort maxPort outputMap debugMode debugOuts

envToDocker :: Env -> String
envToDocker (Env (Program gates _ _) minPort  _ _ _ _) =
  "version: '2'\n\
  \services: \n" <> mconcat (reverse services)
    where 
      services :: [String] 
      services = snd $ foldl (\(p,ss) g -> (p + 1, buildService p g:ss)) (0, []) gates
  
      buildService :: Int -> Gate -> String
      buildService portOffset (Gate tpe name outs) = unlines [
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

      gateUrl :: GateRef -> String
      gateUrl (GateRef name inputNum) = "http://" <> name <> "/in/" <> show inputNum
