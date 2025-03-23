module Angabe1 where
  
import Data.List

type Nat0 = Int
type Nat1 = Int

-- pre: xs is finite
filtere :: Nat1 -> [Int] -> [Int]
filtere n xs
  | n < 1 = error "Nat1 required on param 1"
  | otherwise = filtr n (reverse $ sort xs)

-- pre: xs is finite
filtr :: Nat1 -> [Int] -> [Int]
filtr _ [] = []
filtr n list@(x:_) =
  let
    streakLen = length $ takeWhile (x==) list
  in
    [x | streakLen == n] ++ filtr n (drop streakLen list)

-- NB: destructuring via Patterns (at once List and tuple patterns)
kommt_vor :: Int -> [(Int,Int)] -> Bool
kommt_vor _ [] = False
kommt_vor n ((x,y):rest)
 | n == x || n == y = True
 | otherwise = kommt_vor n rest

aus :: [Int] -> [Int]
aus xs =
  let
    sortedXs = sort xs
    maxOccurance = findMaxOccur sortedXs
    sortedSet = nub sortedXs
  in concat $ map (replicate maxOccurance) sortedSet

-- pre: the equal elements are grouped, i.e. no [1,2,1] but either [1,1,2] or [2,1,1]
findMaxOccur :: Eq a => [a] -> Int
findMaxOccur xs = findMaxOccur' 0 xs

-- pre: initial value of first parameter = 0
findMaxOccur' :: Eq a => Int -> [a] -> Int
findMaxOccur' n [] = n
findMaxOccur' n xs@(x:_) =
  let
    streakLen = length $ takeWhile (x==) xs
    m = max n streakLen
    rest = drop streakLen xs
  in findMaxOccur' m rest
      
-- Hamming distance.
-- NB: running variable for 
hm :: String -> String -> Int
hm s t = h' s t 0

h' :: String -> String -> Int -> Int
h' [] [] n = n
h' _  [] _ = -1
h' [] _  _ = -1
h' (s:ss) (t:ts) n
  | s == t = h' ss ts n
  | s /= t = h' ss ts (n+1)

-- Papier+Bleistift
g :: Int -> Int
g n = if n == 0 then 0 else g (n-1) + 3
h :: Int -> Int
h n = if n == 0 then 0 else 3 + h (n-1)
