module MyLib (run) where

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS
import Parse

run :: IO ()
run = case parse "" "123" of
  Left err -> print err
  Right expr -> putStrLn (BS.unpack (encodePretty expr))
