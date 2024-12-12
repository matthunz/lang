module MyLib (run) where

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS
import Options.Applicative
import Parse
import System.Process

data Args = Args
  { src :: String
  }

args :: Parser Args
args =
  Args
    <$> argument
      str
      ( metavar "SOURCE"
          <> help "Source text to parse"
      )

runParse :: Args -> IO ()
runParse opts = case parse "" (src opts) of
  Left err -> print err
  Right expr ->
    callProcess
      "runtime/target/debug/runtime"
      [BS.unpack (encodePretty expr)]

run :: IO ()
run = runParse =<< execParser opts
  where
    opts =
      info
        (args <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative"
        )
