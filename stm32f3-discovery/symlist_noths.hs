#!/usr/bin/env runghc
import Data.List
import System.Environment

main :: IO ()
main = do
  (a:h:_) <- getArgs
  sall <- readFile a
  shs <- readFile h
  let f = fmap (drop 1 . dropWhile (' ' /=) . drop 1 . dropWhile (' ' /=)) . lines
  putStr $ unlines $ f sall \\ f shs
