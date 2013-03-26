module Main where

import Data.Array.IArray
import Control.Monad

import Data.Char (toUpper)

import System.IO
import System.Environment (getArgs)

import Trie

type Board = Array (Int, Int) Char

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y) =
    [ p | i <- [-1,0,1], j <- [-1,0,1],
        (i, j) /= (0, 0), let p = (x+i, y+j) ]

search :: Trie -> Board -> String -> [(Int, Int)] -> (Int, Int) -> [String]
search dict board str used point = do
  adj <- neighbours point
  -- adj takes on the value of each neighbour in turn as we run the below.
  guard $ inRange (bounds board) adj -- short-circuits and returns []
  guard (adj `elem` used)
  let str' = str ++ [board ! adj]
  guard $ isPrefix dict str'
  let childWords = search dict board str' (point:used) adj
  if isWord dict str'
    then str' : childWords
    else childWords

boggle :: Trie -> Board -> [String]
boggle dict board = concatMap (search dict board [] []) (indices board)


main :: IO ()
main = do
  dictWords <- readFile "/usr/share/dict/words"
  let dict = mkTrie $ lines $ map toUpper dictWords
  [grid] <- getArgs
  let arr = listArray ((1,1), (4,4)) grid
  print $ boggle dict arr
