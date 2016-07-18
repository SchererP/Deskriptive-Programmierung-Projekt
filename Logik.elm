module Logik exposing (..)

import Operationen exposing (..)
import Arithmetic exposing (primesBelow)
import List exposing (foldr, map, head, drop, member, filter, length)


generiereFeld : int -> Sudokufeld
generiereFeld s =
    let
        definiereFeld =
            \pos zahl feld -> setProtected (setField feld pos zahl) pos
    in
        definiereFeld ( 1, 8 )
            1
            (definiereFeld ( 2, 1 )
                4
                (definiereFeld ( 3, 2 )
                    2
                    (definiereFeld ( 4, 5 )
                        5
                        (definiereFeld ( 4, 7 )
                            4
                            (definiereFeld ( 4, 9 )
                                7
                                (definiereFeld ( 5, 3 )
                                    8
                                    (definiereFeld ( 5, 7 )
                                        3
                                        (definiereFeld ( 6, 3 )
                                            1
                                            (definiereFeld ( 6, 5 )
                                                9
                                                (definiereFeld ( 7, 1 )
                                                    3
                                                    (definiereFeld ( 7, 4 )
                                                        4
                                                        (definiereFeld ( 7, 7 )
                                                            2
                                                            (definiereFeld ( 8, 2 )
                                                                5
                                                                (definiereFeld ( 8, 4 ) 1 (definiereFeld ( 9, 4 ) 8 (definiereFeld ( 9, 6 ) 6 emptyField)))
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )



-- Generiert ein eindeutig-lösbares Feld und entfernt ableitbare Einträge
-- Der int-Wert gibt den Schwierigkeitsgrad an


ueberpruefeFeld : Sudokufeld -> Maybe Bool
ueberpruefeFeld feld =
    let
        foldfunction =
            \x y ->
                case x of
                    Nothing ->
                        Nothing

                    Just xbool ->
                        case y of
                            Nothing ->
                                Nothing

                            Just ybool ->
                                Just (ybool && xbool)

        blockerstellung =
            \x -> [ getColumn feld x, getRow feld x, getSubsquare feld x ]
    in
        foldr foldfunction (Just True) (map disjunkteListe (List.concatMap blockerstellung [1..9]))



-- Kontrolliert, ob das Feld vollständig gefüllt und korrekt belegt wurde. Für nicht-vollständige Felder wird Nothing zurückgegeben und sonst Bool


primes : List (Int)
primes =
    1 :: (primesBelow 24)


disjunkteListe : List (Maybe Int) -> Maybe Bool
disjunkteListe x =
    let
        foldfunction =
            \y x ->
                case y of
                    Just i ->
                        case head (drop i primes) of
                            --i-te Primzahl
                            Just item ->
                                item * x

                            Nothing ->
                                29 * x

                    --Sollte nie passieren, 29 ist die nächstgtößere Primzahl
                    Nothing ->
                        0
    in
        case foldr foldfunction 0 x of
            0 ->
                Nothing

            --Mindestens ein Nothing, ein nicht gefülltes Feld
            223092870 ->
                Just True

            --Produkt der ersten neun Primzahlen, somit eindeutig
            _ ->
                Just False



--Mindestens eine Zahl doppelt


moeglicheZiffern : Sudokufeld -> Pos -> List Int
moeglicheZiffern feld pos =
    let
        feldZuteilung =
            ((fst pos - 1) // 3) * 3 + ((snd pos - 1) // 3 + 1)
    in
        let
            filterbedingung y =
                not (member (Just y) (getColumn feld (snd pos))) && not (member (Just y) (getRow feld (fst pos))) && not (member (Just y) (getSubsquare feld feldZuteilung))
        in
            filter filterbedingung [1..9]



-- Liefert eine Liste mit möglichen Einträgen


berechneTipp : Sudokufeld -> Pos -> Maybe (Pos, Int)
berechneTipp feld startpos =
    if fst startpos < 10 && snd startpos < 10 then
        let ziffernliste = moeglicheZiffern feld startpos
        in
            case length ziffernliste of
                0 -> Nothing -- Wenn ein Feld nicht belegt werden kann, ist das Spiel so nicht lösbar
                1 -> Just (startpos, case head ziffernliste of
                                          Just x -> x
                                          Nothing -> 0) -- Rückgabe des eindeutig lösbaren Felds und der entsprechende Eintrag, der Nothing-Fall tritt nie ein
                _ -> case fst startpos of  -- Mehrere Einträge gültig, iteriere über das Feld
                        9 -> berechneTipp feld (1, snd startpos + 1)
                        _ -> berechneTipp feld (fst startpos + 1, snd startpos)
    else
        Nothing -- Das gesamte Feld wurde untersucht, aber es gibt keine eindeutig lösbaren Felder

-- Berechnet eine Position und einen Wert für einen eindeutig lösbaren Eintrag
