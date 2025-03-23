module Angabe5 where

import Data.List (partition, sort, group, find, sortOn, groupBy)
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)

{-# ANN module "HLint: ignore" #-}

type Nat0                = Int
type Nat1                = Int
type Vorname             = String
type Nachname            = String
data Partei              = ABC | DEF | GHI | JKL | MNO deriving (Eq,Show,Ord)
data Wahlwerber          = WW Vorname Nachname Partei deriving (Eq,Show)
type Wahlvorschlag       = [Wahlwerber]
type Wahlvorschlagsplatz = Nat1
type Wahlsieger          = (Wahlwerber,Wahlvorschlagsplatz)
type Stimmzettel         = [Wahlvorschlagsplatz]
type Wahl                = [Stimmzettel]
type Gueltig             = [Stimmzettel]
type Ungueltig           = [Stimmzettel]
type Platz_1_Stimmen     = Nat0
data Wahlausgang         = Ungueltiger_Wahlvorschlag
                           | Keine_gueltigen_Stimmen
                           | Gewaehlt_ist Wahlwerber
                           | Kein_Wahlsieger_Wahlwiederholung deriving (Eq,Show)
data Groesste_Wahlverlierer = GWV [Partei]
                             | Keine 
                             | Analyse_nicht_moeglich deriving (Eq,Show)

-- Aufgabe A.1

ist_gueltiger_Wahlvorschlag :: Wahlvorschlag -> Bool
ist_gueltiger_Wahlvorschlag = not . null

-- Aufgabe A.2

ist_gueltiger_Stimmzettel :: Wahlvorschlag -> Stimmzettel -> Bool
ist_gueltiger_Stimmzettel wahlvorschlag stimmzettel =
    ist_gueltiger_Wahlvorschlag wahlvorschlag
        && length stimmzettel == length (group $ sort stimmzettel)
        && all (\s -> 1 <= s && s <= length wahlvorschlag) stimmzettel

-- Aufgabe A.3

trenne_Stimmzettel :: Wahlvorschlag -> Wahl -> (Gueltig,Ungueltig)
trenne_Stimmzettel wahlvorschlag = partition (ist_gueltiger_Stimmzettel wahlvorschlag)

-- Aufgabe A.4

auszaehlen :: Wahlvorschlag -> Wahl -> Maybe [Platz_1_Stimmen]
auszaehlen wahlvorschlag wahl = 
    if ist_gueltiger_Wahlvorschlag wahlvorschlag && null ungueltig then Just platz_1_stimmen else Nothing
    where
        (gueltige_Stimmzettel, ungueltig) = trenne_Stimmzettel wahlvorschlag wahl

        platz_1_stimmen :: [Platz_1_Stimmen]
        platz_1_stimmen = map (\k -> fromMaybe 0 $ lookup k stimmen_pro_kandidat) [1..length wahlvorschlag]

        stimmen_pro_kandidat :: [(Wahlvorschlagsplatz, Platz_1_Stimmen)]
        stimmen_pro_kandidat = 
            -- jede der Liste wird zusammengefasst
            map (\stimmen@(kandidat:_) -> (kandidat, length stimmen))
                -- sortieren und Gruppieren
                -- dadurch entsteht eine Liste von Listen mit gleichen stimmen
                . group
                . sort
                -- Erste Reihung jedes Stimmzettels bestimmen
                -- Enthaltungen filtern
                . mapMaybe listToMaybe
                $ gueltige_Stimmzettel

-- Aufgabe A.5

wahlsieger :: Wahlvorschlag -> Maybe [Platz_1_Stimmen] -> Maybe Wahlsieger
wahlsieger _ Nothing = Nothing
wahlsieger wahlvorschlag (Just platz_1_stimmen) = 
    case platz_1_gewinner of
        Nothing -> Nothing
        Just (gewinner, _) -> Just (wahlvorschlag !! (gewinner - 1), gewinner)
    where
        platz_1_gewinner = 
            find (\(_, stimmen) -> 2*stimmen > sum platz_1_stimmen) 
                . zip [1..] 
                $ platz_1_stimmen


-- Aufgabe A.6

ausscheiden :: Wahl -> [Platz_1_Stimmen] -> Wahl
ausscheiden wahl platz_1_stimmen =
    map (filter ist_nicht_verlierer) wahl
    where
        ist_nicht_verlierer :: Wahlvorschlagsplatz -> Bool
        ist_nicht_verlierer k = not $ k `elem` verlierer

        verlierer =
            map fst
                . head
                . groupBy (\(_,s1) (_,s2) -> s1 == s2)
                . sortOn snd
                . zip [1..]
                $ platz_1_stimmen

-- Aufgabe A.7

wahlausgang :: Wahlvorschlag -> Wahl -> Wahlausgang
wahlausgang wahlvorschlag wahl 
    | not $ ist_gueltiger_Wahlvorschlag wahlvorschlag = Ungueltiger_Wahlvorschlag
    | null gueltige_Stimmzettel = Keine_gueltigen_Stimmen
    | otherwise = wahlausgang2 wahlvorschlag [1..length wahlvorschlag] gueltige_Stimmzettel
    where
        (gueltige_Stimmzettel, _) = trenne_Stimmzettel wahlvorschlag wahl

-- `wahlausgang`, aber mit der Zusatzinformation welche Kandidaten noch im Rennen sind
-- 
-- Vorbedingungen
-- * Wahlvorschlag ist gueltig
-- * Alle stimmzettel in der Wahl sind gueltig
wahlausgang2 :: Wahlvorschlag -> [Wahlvorschlagsplatz] -> Wahl -> Wahlausgang
wahlausgang2 wahlvorschlag noch_im_rennen wahl 
    | null noch_im_rennen = Kein_Wahlsieger_Wahlwiederholung
    | Just (sieger, _) <- wahlsieger wahlvorschlag (Just platz_1_Stimmen) = Gewaehlt_ist sieger
    | otherwise = wahlausgang2 wahlvorschlag immer_noch_im_rennen naechster_durchgang
    where
        Just platz_1_Stimmen = auszaehlen wahlvorschlag wahl

        (naechster_durchgang, immer_noch_im_rennen) = ausscheiden2 noch_im_rennen wahl platz_1_Stimmen
        
-- `ausscheiden`, aber mit der Zusatzinformation welche Kadidaten noch im Rennen sind
ausscheiden2 :: [Wahlvorschlagsplatz] ->  Wahl -> [Platz_1_Stimmen] -> (Wahl, [Wahlvorschlagsplatz])
ausscheiden2 noch_im_rennen wahl platz_1_stimmen =
    ( map (filter ist_nicht_verlierer) wahl
    , filter ist_nicht_verlierer noch_im_rennen
    )
    where
        ist_nicht_verlierer :: Wahlvorschlagsplatz -> Bool
        ist_nicht_verlierer k = not $ k `elem` verlierer
        verlierer =
            map fst
                . head
                . groupBy (\(_,s1) (_,s2) -> s1 == s2)
                . sortOn snd
                . filter (\(k, _) -> k `elem` noch_im_rennen)
                . zip [1..]
                $ platz_1_stimmen

-- Aufgabe A.8

wahlanalyse :: Wahlvorschlag -> Wahl -> Groesste_Wahlverlierer
wahlanalyse wahlvorschlag wahl
    | not $ ist_gueltiger_Wahlvorschlag wahlvorschlag = Analyse_nicht_moeglich
    | length parteien == 1 = Keine
    | otherwise = wahlanalyse2 wahlvorschlag parteien [1..length wahlvorschlag] gueltige_Stimmzettel
    where
        (gueltige_Stimmzettel, _) = trenne_Stimmzettel wahlvorschlag wahl
        parteien = map head . group . sort . map (\(WW _ _ p) -> p) $ wahlvorschlag

-- `wahlanalyse`, aber mit der Zusatzinformation:
-- * welche Kandidaten noch im Rennen sind
-- * welche parteien angetreten sind
-- 
-- Vorbedingungen
--  * Wahlvorschlag ist gueltig
--  * Es treten mehr als eine Partei an
--  * Alle stimmzettel in der Wahl sind gueltig
wahlanalyse2 :: Wahlvorschlag -> [Partei] -> [Wahlvorschlagsplatz] -> Wahl -> Groesste_Wahlverlierer
wahlanalyse2 wahlvorschlag parteien noch_im_rennen wahl 
    | null noch_im_rennen = GWV parteien -- alle haben gleichzeitig verloren
    | not $ null verlorene_parteien = GWV verlorene_parteien
    | otherwise = wahlanalyse2 wahlvorschlag parteien immer_noch_im_rennen naechster_durchgang
    where
        Just platz_1_Stimmen = auszaehlen wahlvorschlag wahl

        (naechster_durchgang, immer_noch_im_rennen) = ausscheiden2 noch_im_rennen wahl platz_1_Stimmen

        parteien_noch_im_rennen = 
            map head 
                . group 
                . sort 
                . map (\(WW _ _ p) -> p) 
                . map (\k -> wahlvorschlag !! (k - 1))
                $ immer_noch_im_rennen

        verlorene_parteien = filter (not . (`elem` parteien_noch_im_rennen)) parteien
