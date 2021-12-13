module Interpreter.Docker(DockerEnv(..), dockerComposeProgram, dockerEnvParser) where

import Options.Applicative

import Data.List
import Data.List.Split

import Interpreter.Resolver(Program(..), Gate(..), GateRef(..))

import qualified Data.Map.Strict as Map

data DockerEnv = DockerEnv {
  _portMin :: Int,
  _portMax :: Int,
  _outputMapping :: Map.Map String String,
  _debugMode :: Bool,
  _debugOutputs :: [String]
}

dockerEnvParser :: ParserInfo DockerEnv
dockerEnvParser = info (envParser <**> helper) (progDesc "Create a docker-compose gateaas cluster file")
  where
    envParser = DockerEnv
      <$> option auto (long "min-port" <> metavar "PORT" <> help "The minimum port to bind to in the docker compose output.") 
      <*> option auto (long "max-port" <> metavar "PORT" <> help "The maximum port to bind to in the docker compose output.") 
      <*> outputMappingParser
      <*> switch (long "debug-mode" <> short 'd' <> help "If enabled, all docker containers will bind to a host port, enabling every gate to be probed directly.") 
      <*> many (strOption (long "debug-output" <> metavar "DEBUG_OUTPUT" <> help "If the debug-mode flag is set, all specified URLs will be sent the output of all gates that have a port bound to the host."))
    outputMappingParser :: Parser (Map.Map String String)
    outputMappingParser = Map.fromList <$> many (option (eitherReader buildPair) $ long "gate-output" <> short 'g' <> metavar "$OUTPUT=$URL" <> help "Sends the result of $OUTPUT to $URL")

    buildPair :: String -> Either String (String, String)
    buildPair s = case wordsBy (=='=') s of
      h:t:[] -> Right (h, t)
      lst -> Left $ "Malformed output mapping specified. Expected $OUTPUT=$URL, got '" <> intercalate "=" lst <> "'"

dockerComposeProgram :: DockerEnv -> Program -> String
dockerComposeProgram (DockerEnv minPort  _ _ _ _) (Program gates _ _) =
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
