module Angabe6 where

import Data.List (groupBy)

{-# ANN module "HLint: ignore" #-}

-- Aufgabe A.1

gen_sort :: (a -> a -> Bool) -> [a] -> [a]
gen_sort cmp xs = loop [] xs
    where
        -- loop :: [a] -> [a] -> [a]
        loop sorted [] = sorted
        loop sorted (x:xs) = loop (gen_insert cmp x sorted) xs

gen_insert :: (a -> a -> Bool) -> a -> [a] -> [a]
gen_insert _ x [] = [x]
gen_insert cmp x (a:as) = if x `cmp` a then x:a:as else a:gen_insert cmp x as

-- Aufgabe A.3

auf_ord :: Ord a => [a] -> [a]
auf_ord = gen_sort (<)

ab_ord  :: Ord a => [a] -> [a]
ab_ord = gen_sort (>)

-- Aufgabe A.8

auf_lst :: [[a]] -> [[a]]
auf_lst = gen_sort (\l1 l2 -> length l1 < length l2)

ab_lst  :: [[a]] -> [[a]]
ab_lst = gen_sort (\l1 l2 -> length l1 > length l2)

-- Aufgabe A.10

auf_fun :: [Int -> Int] -> [Int -> Int]
auf_fun = gen_sort (\f1 f2 -> f1 0 < f2 0)

ab_fun  :: [Int -> Int] -> [Int -> Int]
ab_fun = gen_sort (\f1 f2 -> f1 0 > f2 0)

-- Aufgabe A.11

auf_onfun :: (Ord a,Num a) => [a -> a] -> [a -> a]
auf_onfun = gen_sort (\f1 f2 -> f1 (fromInteger 0) < f2 (fromInteger 0))

ab_onfun  :: (Ord a,Num a) => [a -> a] -> [a -> a]
ab_onfun = gen_sort (\f1 f2 -> f1 (fromInteger 0) > f2 (fromInteger 0))

-- Aufgabe A.12

auf_nfun :: (Eq a, Num a) => [a -> a] -> [a -> a]
auf_nfun = gen_sort (\f1 f2 -> not $ f1 (fromInteger 0) `num_greater_than` f2 (fromInteger 0))

ab_nfun :: (Eq a, Num a) => [a -> a] -> [a -> a]
ab_nfun = gen_sort (\f1 f2 -> f1 (fromInteger 0) `num_greater_than` f2 (fromInteger 0))

-- vergleicht ob eine Zahl kleiner oder Groesser als die andere ist.
-- Annahme: `signum` nimmt für positive Werte das Resultat `1`.
num_greater_than :: (Eq a, Num a) => a -> a -> Bool
num_greater_than a b = signum (a - b) == signum (fromInteger 1)

-- Aufgabe A.13

type Nat1               = Int
type Name               = String
type Alter              = Nat1
data Geschlecht         = M | F | D deriving (Eq,Ord,Show)
type Gehalt             = Nat1
type PersNummer         = Int
data Hersteller         = Alcatel | Apple | Huawai | LG | Motorola 
                          | Nokia | Samsung deriving (Eq,Ord,Show)
type Hat_Smartphone_von = Maybe Hersteller
data Person             = P PersNummer Name Alter Geschlecht Gehalt
                            Hat_Smartphone_von deriving (Eq,Show)
type Datenbank          = [Person]
type Nutzungssicht      = Datenbank

normalsicht :: Datenbank -> Nutzungssicht
normalsicht = 
    gen_sort (\(P _ name1 _ _ _ _) (P _ name2 _ _ _ _) -> name1 < name2)

anlageberatungssicht :: Datenbank -> Nutzungssicht
anlageberatungssicht = 
    gen_sort (\(P _ _ _ _ gehalt1 _) (P _ _ _ _ gehalt2 _) -> gehalt1 > gehalt2)

personalabteilungssicht :: Datenbank -> Nutzungssicht
personalabteilungssicht = gen_sort (\(P _ _ alter1 geschlecht1 _ _) (P _ _ alter2 geschlecht2 _ _) -> 
        combine_cmp (>) (<) (geschlecht1, alter1) (geschlecht2, alter2)
    )

sozialforschungssicht :: Datenbank -> Nutzungssicht
sozialforschungssicht = 
    gen_sort (\(P _ _ _ _ gehalt1 smartphone1) (P _ _ _ _ gehalt2 smartphone2) -> 
        combine_cmp (cmp_nothings_last (<)) (>) (smartphone1, gehalt1) (smartphone2, gehalt2)
    )

integritaetssicht :: Datenbank -> Nutzungssicht
integritaetssicht = 
    concat
        . gen_sort (\g1 g2 -> combine_cmp (>) (<) 
            (length g1, (\(P nr _ _ _ _ _) -> nr) $ head g1) (length g2, (\(P nr _ _ _ _ _) -> nr) $ head g2))
        . groupBy (\(P nr1 _ _ _ _ _) (P nr2 _ _ _ _ _) -> nr1 == nr2)
        . gen_sort (\(P nr1 _ _ _ _ _) (P nr2 _ _ _ _ _) -> nr1 < nr2 )

auch_im_chaos_ist_ordnung_sicht :: Datenbank -> Nutzungssicht
auch_im_chaos_ist_ordnung_sicht =
    gen_sort (\(P _ name1 _ _ _ _) (P _ name2 _ _ _ _) -> head (gen_sort (<) name1) < head (gen_sort (<) name2))

combine_cmp :: Eq a => (a -> a -> Bool) -> (b -> b -> Bool) -> (a, b) -> (a, b) -> Bool
combine_cmp cmp1 cmp2 (a1, b1) (a2, b2) = a1 `cmp1` a2 || (a1 == a2 && b1 `cmp2` b2)

cmp_nothings_last :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
cmp_nothings_last _ Nothing _ = False
cmp_nothings_last _ _ Nothing = True
cmp_nothings_last cmp (Just a1) (Just a2) = a1 `cmp` a2
