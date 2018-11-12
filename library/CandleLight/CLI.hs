module CandleLight.CLI where

import CandleLight.Repl
import CandleLight.Interpreter

import Options.Applicative
import Data.Semigroup ((<>))

data Entry = Repl | Run FilePath

compileParse :: Parser Entry
compileParse = Run <$> (strOption (long "file" <> metavar "TARGET" <> help "Target for compilation") <|> argument str (metavar "TARGET" <> help "Target for compilation"))

replParse :: Parser Entry
replParse = flag' Repl (long "repl" <> help "Runs the read evaluate print loop")

cliParse :: Parser Entry
cliParse = compileParse <|> cliParse

useEntry :: Entry -> IO ()
useEntry Repl = repl
useEntry (Run filename) = interpret filename

cli :: IO ()
cli = execParser opts >>= useEntry
    where
        opts = info (cliParse <**> helper) (fullDesc <> progDesc "The CandleLight CLI Interface" <> header "CandleLight")
