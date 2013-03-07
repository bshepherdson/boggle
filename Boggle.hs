module Main where

import Data.Array.IArray
import Control.Monad

import Data.Char (toUpper)

import System.IO
import System.Environment (getArgs)

import Trie

type Board = Array (Int, Int) Char

adjacent :: (Int, Int) -> (Int, Int) -> Bool
adjacent (a,b) (c,d) = close (a-c) && close (b-d)
  where close x = abs x <= 1

neighbours :: Board -> (Int, Int) -> [(Int, Int)]
neighbours board point = filter (/= point) $ filter (adjacent point) (indices board)

search :: Trie -> Board -> String -> (Int, Int) -> [String]
search dict board str point = do
  adj <- neighbours board point
  let str' = str ++ [board ! adj]
  guard $ isPrefix dict str' -- short-circuits and returns []
  let childWords = search dict board str' adj
  if isWord dict str'
    then str' : childWords
    else childWords

boggle :: Trie -> Board -> [String]
boggle dict board = concatMap (search dict board []) (indices board)


main :: IO ()
main = do
  dictWords <- readFile "/usr/share/dict/words"
  let dict = mkTrie $ lines $ map toUpper dictWords
  [grid] <- getArgs
  let arr = listArray ((1,1), (4,4)) grid
  print $ boggle dict arr
