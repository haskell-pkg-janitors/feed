module Main where

import Text.XML
import Text.Feed.Import
import Text.Feed.Export

import Data.Text.Lazy
import System.Environment

main :: IO ()
main = do
  (x:_) <- getArgs
  feed  <- parseFeedFromFile x
  putStrLn $ unpack $ renderText def $ Document defprologue (xmlFeed feed) defepilogue
  where
    defprologue = Prologue [] Nothing []
    defepilogue = []