module MyLib (run) where

import Parse

run :: IO ()
run = case parse "" "123" of
  Left err -> print err
  Right expr -> do
    print expr
