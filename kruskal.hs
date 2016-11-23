module Main where

import System.Environment
import Data.Maybe
import Graph
import MST.Kruskal

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  let graph = toGraph file
  let mst'  = mst graph
  if isNothing mst'
    then putStrLn "Impossible"
    else putStrLn . formatGraph $ fromJust mst'
  return ()
