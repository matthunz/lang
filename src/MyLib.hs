module MyLib (run) where

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS
import Options.Applicative
import Parse

data Sample = Sample
  { src :: String
  }

sample :: Parser Sample
sample =
  Sample
    <$> argument
      str
      ( metavar "SOURCE"
          <> help "Source text to parse"
      )

runParse :: Sample -> IO ()
runParse opts = case parse "" (src opts) of
  Left err -> print err
  Right expr -> putStrLn (BS.unpack (encodePretty expr))

run :: IO ()
run = runParse =<< execParser opts
  where
    opts =
      info
        (sample <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative"
        )
