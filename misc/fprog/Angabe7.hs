module Angabe7 where

import Data.List (intercalate, find, group, sort)

{-# ANN module "HLint: ignore" #-}

type Nat0 = Integer
type Zett = Integer

type Zeichenvorrat = Char
data Bandalphabet  = Z Zeichenvorrat 
                     | Blank deriving (Eq,Show,Ord)
                      
-- Rechenband
type Min        = Zett -- Kleinster Index eines beschriebenen Bandfelds
type Max        = Zett -- Groesster Index eines beschriebenen Bandfelds
data MinMax     = B Min Max                  -- B wie `Beschrieben'
                  | U deriving (Eq,Show)     -- U wie `Unbeschrieben'  
type Bandfeld   = Zett
type Band       = (Bandfeld -> Bandalphabet) 
data Rechenband = RB MinMax Band

-- Lese- und Schreibkopf (LSK)
type LSK_Position      = Zett
type Zeichen_unter_LSK = Bandalphabet
data Richtung          = Links | Rechts deriving (Eq,Show)
data Befehl            = Drucke Bandalphabet 
                          | Bewege_LSK_nach Richtung deriving (Eq,Show)

-- Interne Turingmaschinenzustaende
type Zustand               = Nat0
type Interner_Zustand      = Zustand
type Interner_Folgezustand = Zustand

-- Abkuerzungen
type LSKZ = Zeichen_unter_LSK
type IZ   = Interner_Zustand
type IFZ  = Interner_Folgezustand 

-- Turing-Tafeln
type Zeile       = (IZ,LSKZ,Befehl,IFZ)
type Turingtafel = [Zeile]

-- Globale Turingmaschinenzustaende
data GZustand = GZ Turingtafel Rechenband IZ LSK_Position

-- Spuren
type Spur = [GZustand]

 -- Turingmaschinensimulatoreingabe
type Initiales_Rechenband = Rechenband
data Sim_Eingabe = SE Turingtafel Initiales_Rechenband

-- Turingmaschinensimulatorausgabe
type Finaler_interner_Zustand = Zustand
type Finale_LSK_Position      = LSK_Position
type Finales_Rechenband       = Rechenband

-- Abkuerzungen
type FIZ   = Finaler_interner_Zustand
type FLSKP = Finale_LSK_Position
type FRB   = Finales_Rechenband

-- Simulatorausgabe
data Sim_Ausgabe = SA FIZ FLSKP FRB

-- Aufgabe A.1

akt_band :: Band -> Bandfeld -> Bandalphabet -> Band
akt_band band feld c = \f -> if f == feld then c else band f

akt_rechenband :: Rechenband -> Bandfeld -> Bandalphabet -> Rechenband
akt_rechenband rb@(RB U _) _ Blank = rb
akt_rechenband (RB U _) feld c = RB (B feld feld) $ \f -> if f == feld then c else Blank
akt_rechenband (RB (B min max) band) feld c =
    RB newMinMax $ akt_band band feld c
    where
        findNewBound boundConstructor range = 
            maybe U boundConstructor $ find (\m -> band m /= Blank) range
        newMinMax 
            | c == Blank && feld == min = findNewBound (\m -> B m max) [min+1..max]
            | c == Blank && feld == max = findNewBound (\m -> B min m) [max-1,max-2..min]
            | c /= Blank && feld < min = B feld max
            | c /= Blank && feld > max = B min feld
            | otherwise = B min max

-- -- Aufgabe A.2 (a) bis (h)

wandle_in_rb :: [Zeichenvorrat] -> Rechenband
wandle_in_rb [] = RB U $ const Blank
wandle_in_rb band = RB (B 1 max) $ \f -> if 1 <= f && f <= max then Z (band !! (fromIntegral $ f-1)) else Blank
    where
        max = fromIntegral $ length band

-- Zulaessige Turingtafeln
ist_zulaessige_Turingtafel :: Turingtafel -> Bool
ist_zulaessige_Turingtafel [] = True
ist_zulaessige_Turingtafel tafel = 
    maximum (map length . group . sort $ map (\(iz,z,_,_) -> (iz,z)) tafel) == 1

transition :: GZustand -> GZustand
transition (gz@(GZ tafel (rb@(RB _ band)) zustand pos)) = GZ tafel rb_neu zustand_neu pos_neu
    where
        (rb_neu, zustand_neu, pos_neu) = case aktive_zeile gz of
            Nothing -> (rb, zustand, pos)
            Just (_,_,Drucke x,z2) -> (akt_rechenband rb pos x, z2, pos)
            Just (_,_,Bewege_LSK_nach Links, z2) -> (rb, z2, pos-1)
            Just (_,_,Bewege_LSK_nach Rechts, z2) -> (rb, z2, pos+1)

aktive_zeile :: GZustand -> Maybe Zeile
aktive_zeile (GZ tafel (RB _ band) zustand pos) = 
    find (\(iz,z,_,_) -> iz == zustand && z == zeichen) tafel
    where
        zeichen = band pos

spur :: GZustand -> Spur
spur gz = gz : case aktive_zeile gz of
    Nothing -> []
    Just _ -> spur folgezustand
    where
        folgezustand = transition gz

zeige_zustand :: GZustand -> String
zeige_zustand (GZ _ (RB minmax band) zustand pos) =
    "(IZ:" ++ show zustand ++ ",LSK:" ++ show pos ++ ",B:" ++ bandtext ++ ")"
    where
        bandtext = case minmax of
            U -> "unbeschrieben"
            B min max -> 
                map ((\(Z x) -> x) . band) [min..max] 
                    ++ ",Min:" 
                    ++ show min 
                    ++ ",Max:" 
                    ++ show max

zeige_spur :: Spur -> String
zeige_spur = intercalate "->>" . map zeige_zustand

sim :: Sim_Eingabe -> Sim_Ausgabe
sim (SE tafel band) = 
    SA z pos b
    where
        GZ _ b z pos = last . spur $ GZ tafel band 0 0

instance Show Sim_Ausgabe where
    show (SA z pos band) = "IZ: " ++ show z ++ " // LSKP: " ++ show pos ++ " // BI: " ++ bandString
        where
            bandString = case band of
                (RB U _) -> "Leer"
                (RB (B min max) b) -> show min ++ ">" ++ map ((\(Z x) -> x) . b) [min..max] ++ "<" ++ show max
