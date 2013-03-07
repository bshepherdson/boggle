module Trie where

import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.List

data Trie = Trie Bool (M.Map Char Trie)

isWord :: Trie -> String -> Bool
isWord t s = case getNode t s of
               Just (Trie b _) -> b
               Nothing -> False

isPrefix :: Trie -> String -> Bool
isPrefix t s = isJust $ getNode t s


getNode :: Trie -> String -> Maybe Trie
getNode t [] = Just t
getNode t@(Trie b m) (c:s) = case M.lookup c m of
  Nothing -> Nothing
  Just t' -> getNode t' s



-- building a trie
-- recursive groupBy and then building Tries bottom-up
mkTrie :: [String] -> Trie
mkTrie = mkTrie' . sort
  where mkTrie' :: [String] -> Trie
        mkTrie' ([]:strs) = case mkTrie' strs of (Trie _ m) -> Trie True m
        mkTrie' strs = let m = M.fromList
                             $ map (\x -> (head (head x), mkTrie' (map tail x)))
                             $ groupBy (comparing head) strs
                       in Trie False m

comparing :: Eq a => (b -> a) -> b -> b -> Bool
comparing f x y = f x == f y
