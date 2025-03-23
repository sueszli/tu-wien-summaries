module Angabe2 where
import Data.List

type Nat0 = Integer

-- Aufgabe A.1

data IstEinsBinaerPrim = Ja | Nein deriving (Eq,Show)

{- 
  1) calculate binärdarstellung / count # of 1s
  2) check if # of 1s is prime
 -}
ist_einsbp :: Nat0 -> IstEinsBinaerPrim
ist_einsbp n
  | elem numberOf1s relevPrimes = Ja
  | otherwise = Nein
  where numberOf1s = countBin1s n
        relevPrimes = takeWhile (<= numberOf1s) primes
-- side-quest: pull out a "isPrime" predicate from ist_einsbp
-- side-quest: create generic version of "ist_----bp"

countBin1s :: Nat0 -> Nat0
countBin1s 0 = 0
countBin1s 1 = 1
countBin1s n = mod n 2 + countBin1s (n `div` 2)

-- alternative, via an "explicit" binary-representation
-- also yields a simple impl. of countBin0s

countBin1s' n = countOccurances True $ binaryRep n
countBin0s n = countOccurances False $ binaryRep n

ist_nullbp n 
  | elem numberOf0s relevPrimes = Ja
  | otherwise = Nein
  where numberOf0s = countBin0s n
        relevPrimes = takeWhile (<= numberOf0s) primes

-- returns a binary representation of the non-negative paramter
-- True = 1, False = 0. 
-- "little-endian" order, i.e. 8 = [F,F,T]
binaryRep :: Nat0 -> [Bool]
binaryRep 0 = [False]
binaryRep 1 = [True]
binaryRep n = (mod n 2 == 1) : binaryRep (n `div` 2)

countOccurances :: Eq a => a -> [a] -> Integer
countOccurances e [] = 0
countOccurances e (x:xs)
  | e == x = 1 + countOccurances e xs
  | otherwise = countOccurances e xs

-- copied from Prof's slides
primes :: [Integer]
primes = sieve [2..]
sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]

-- Aufgabe A.2

type Von           = Nat0
type Bis           = Nat0
type VonBis        = (Von,Bis)
type Anzahl0bps    = Int
type Anzahl1bps    = Int
type Anzahlen01bps = (Anzahl0bps,Anzahl1bps)

-- just shields against improper arguments
anz01bps :: VonBis -> Anzahlen01bps
anz01bps (v,b)
  | v > b = (-1,-1)
  | otherwise = count01bps [v..b]

-- counts the number of nullbp and einsbp numbers in a list of natural numbers
count01bps :: [Nat0] -> Anzahlen01bps
count01bps []  = (0,0)
count01bps (x:xs) = let
    i_0 = fst (count01bps xs)
    i_1 = snd (count01bps xs)
    step_0 = case ist_nullbp x of Ja   -> 1
                                  Nein -> 0
    step_1 = case ist_einsbp x of Ja   -> 1
                                  Nein -> 0
  in
    (i_0 + step_0, i_1 + step_1)

-- Aufgabe A.3

type Wort      = String
type Wortliste = [Wort]

liefere_woerter_in :: String -> Wortliste
liefere_woerter_in = lwi

lwi :: String -> Wortliste
lwi = filter (/= []) . splitOn isWS

-- NB: Pattern matching works on Chars too
-- NB: Whitespace like newline or tab are Chars too
isWS :: Char -> Bool
isWS ' '  = True
isWS '\n' = True
isWS '\t' = True
isWS _    = False

-- gives all pairs of a list's elements
--    e.g. allPairs [1,2,3] ->> [(1,2),(1,3),(2,3)]
-- not including "self" duplicates
--    i.e. allPairs [1,2] doesn't contain (1,1) or (2,2)
-- or the flipped "duplicates"
--    i.e. allPairs [1,2] doesn't contain both (1,2) and (2,1)
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn f [] = []
splitOn f xs = let
  (run,rest) = break f xs
  in run : splitOn f (drop 1 rest)

-- Aufgabe A.4

type Hammingabstand = Int

-- 1st step:
-- filters undefined cases (too few strings/strings of different lengths)
hM :: [String] -> Hammingabstand
hM ss
  | length ss < 2     = -1
  | differentLengths  = -1
  | otherwise         = hM' ss
  where
    fstStringLength = length $ head ss
    differentLengths = any ((fstStringLength /=) . length) ss
                    -- any (/= fstStringLength) $ map length ss 

hM' :: Eq a => [[a]] -> Int
hM' xss
  | res == 0 = -1
  | otherwise = res
  where res = minimum [pairHM a b | (a,b) <- allPairs xss]

-- reusable
-- gives all pairs of a list's elements
--    e.g. allPairs [1,2,3] ->> [(1,2),(1,3),(2,3)]
-- not including "self" duplicates
--    i.e. allPairs [1,2] doesn't contain (1,1) or (2,2)
-- or the flipped "duplicates"
--    i.e. allPairs [1,2] doesn't contain both (1,2) and (2,1)
allPairs :: [a] -> [(a,a)]
allPairs [] =  error "no pairs constructable: length param_list == 0"
allPairs [x] = error "no pairs constructable: length param_list == 1"
allPairs xs = [ (a,b) | i <- [0..length xs - 2],
                        let subl = drop i xs,
                        let a = head subl,
                        b <- drop 1 subl
              ]

-- returns the Hamming-Distance of two lists
-- looking at just the first "min (length fstList) (length sndList)" elements of the lists
pairHM :: Eq a => [a] -> [a] -> Int
pairHM = pairHM_rec 0

pairHM_rec :: Eq a => Int -> [a] -> [a] -> Int
pairHM_rec n [] _ = n
pairHM_rec n _ [] = n
pairHM_rec n (x:xs) (y:ys)
  | x == y    = pairHM_rec     n xs ys
  | otherwise = pairHM_rec (n+1) xs ys

