{-# LANGUAGE TypeFamilies #-}
module Main where

import Options.Applicative

import Data.List
import Data.Bifunctor
import Data.Functor.Foldable(cata)

import Model (BoolExpr, BoolExprF(..))
import Parser
import Interpreter.Docker(dockerComposeProgram, dockerEnvParser, DockerEnv)
import Interpreter.Resolver(resolveBoolExpr)

main :: IO ()
main = do
  (Opts inFile outFile mode) <- execParser optParser
  programString <- readFile inFile
  let parseResult = parseString programString 
  print parseResult
  let mostParsed = fst $ last (fst parseResult)
  let program = first (\es -> "References to variables before they are defined: " <> intercalate "," es) (resolveBoolExpr mostParsed)
  result <- case mode of
    Docker env -> pure $ dockerComposeProgram env <$> program
    ProgString _ -> pure $ Right $ progToString mostParsed
  either error (write outFile) result
    where
      write :: Maybe String -> String -> IO ()
      write Nothing s = putStrLn s
      write (Just name) s = 
        do 
          writeFile name s
          putStrLn $ "Successfully wrote to " <> name

data Opts = Opts {
  _inputFile :: String,
  _outputFile :: Maybe String,
  _mode :: Mode
}

data Mode
  = Docker DockerEnv
  | ProgString ProgToStringEnv

optParser :: ParserInfo Opts
optParser = info 
    (Opts 
      <$> strArgument (help "The gateass file to read" <> metavar "INPUT_FILE")
      <*> optional (option auto (long "output-file" <> short 'o' <> help "The location to write output. Omit for stdout" <> metavar "OUTPUT_FILE"))
      <*> envParser <**> helper)
    (fullDesc <> progDesc "Parse gateaas logic and output some format" <> header "gateaas-compiler A compiler that converts logic in to gates")

envParser :: Parser Mode
envParser = subparser
    (command "docker" (Docker <$> dockerEnvParser)
    <> command "string"  (ProgString <$> progToStringParser)
    )

data ProgToStringEnv = ProgToStringEnv Int

progToStringParser :: ParserInfo ProgToStringEnv
progToStringParser = info (parser <**> helper) (progDesc "Output the program as parsed")
  where
    parser = ProgToStringEnv <$> option auto (long "indent-level" <> short 'l')

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

