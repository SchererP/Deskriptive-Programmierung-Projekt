module Logik exposing (..)

import Operationen exposing (..)
import Arithmetic exposing (primesBelow)
import List exposing (foldr, map, map2, head, drop, member, filter, length, concat, concatMap, repeat)


generiereFeld : Int -> Sudokufeld
generiereFeld s =
    let
        definiereFeld =
            \pos zahl feld -> setProtected (setField feld pos zahl) pos
        inputSudoku =
                foldr
                    (\poswithnumber ->
                        if snd poswithnumber == 0 then
                            \x -> x
                        else
                            definiereFeld (fst poswithnumber) (snd poswithnumber)
                    )
                    emptyField
        in
            inputSudoku (map2 (,) (map2 (,) (concatMap (repeat 9) [1..9]) (concat (repeat 9 [1..9]))) (getSudokuFeld s))



--Das erste map verbinded position und wert, das zweite erstellt die positionen


getSudokuFeld : Int -> List Int
getSudokuFeld s =
    let
        sudokus =
            [ [ 0, 0, 0, 0, 0, 0, 0, 1, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 4, 0, 7, 0, 0, 8, 0, 0, 0, 3, 0, 0, 0, 0, 1, 0, 9, 0, 0, 0, 0, 3, 0, 0, 4, 0, 0, 2, 0, 0, 0, 5, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 6, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 6, 0, 4, 0, 0, 8, 0, 0, 0, 3, 0, 0, 0, 0, 1, 0, 9, 0, 0, 0, 0, 3, 0, 0, 4, 0, 0, 2, 0, 0, 0, 5, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 7, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 3, 5, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 7, 0, 7, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 4, 0, 0, 8, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 4, 0, 0, 5, 0, 0, 0, 0, 6, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 3, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 4, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 3, 0, 0, 7, 0, 0, 0, 0, 0, 6, 0, 0, 2, 8, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 3, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 8, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 1, 2, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 7, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 5, 0, 7, 0, 0, 0, 3, 0, 0, 0, 0, 0, 6, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 4, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 7, 0, 6, 0, 0, 4, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 8, 7, 5, 0, 0, 6, 0, 1, 0, 0, 0, 3, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 5, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 7, 0, 0, 6, 0, 0, 4, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 9, 2, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 5, 1, 0, 7, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 1, 0, 7, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 5, 0, 4, 0, 0, 0, 0, 1, 4, 0, 0, 8, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 4, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 7, 0, 2, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 1, 0, 8, 0, 0, 0, 0, 1, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 7, 0, 0, 5, 0, 2, 0, 0, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 5, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 6, 0, 0, 1, 2, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 4, 5, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 5, 0, 0, 7, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 7, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 8, 0, 2, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 1, 0, 9, 0, 0, 0, 0, 1, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 8, 0, 0, 5, 0, 2, 0, 0, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 8, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 9, 0, 2, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 5, 0, 1, 0, 0, 0, 0, 1, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 9, 0, 0, 6, 0, 2, 0, 0, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 9, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 1, 0, 0, 7, 0, 0, 0, 8, 0, 4, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 3, 0, 0, 0, 0, 3, 0, 0, 8, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 6, 0, 0, 0, 0, 3, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 6, 0, 0, 5, 0, 0, 2, 0, 4, 0, 0, 0, 4, 0, 0, 7, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 3, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 7, 6, 0, 2, 0, 0, 0, 0, 8, 0, 0, 0, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 7, 5, 0, 6, 0, 0, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 3, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 0, 8, 0, 2, 0, 0, 0, 0, 0, 0, 4, 0, 0, 9, 0, 0, 1, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 8, 9, 0, 0, 0, 0, 0, 5, 0, 0, 4, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 3, 0, 0, 0, 7, 0, 0, 0, 6, 0, 0, 0, 0, 5, 0, 8, 0, 0, 0, 0, 0, 0, 4, 0, 0, 8, 0, 0, 1, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 7, 4, 0, 0, 0, 0, 0, 5, 0, 0, 2, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 3, 0, 0, 0, 7, 0, 0, 0, 6, 0, 0, 0, 0, 5, 0, 9, 0, 0, 0, 0, 0, 0, 4, 0, 0, 9, 0, 0, 1, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 7, 4, 0, 0, 0, 0, 0, 5, 0, 0, 8, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 3, 0, 0, 0, 8, 0, 0, 0, 7, 0, 0, 0, 0, 5, 0, 2, 0, 0, 0, 0, 0, 0, 4, 0, 0, 9, 0, 0, 1, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 8, 9, 0, 0, 0, 0, 0, 5, 0, 0, 4, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 1, 3, 0, 2, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 3, 0, 0, 0, 0, 7, 0, 0, 0, 0, 8, 0, 2, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 4, 0, 5, 0, 0, 6, 7, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ]
            ]
    in
        case head (drop (s - 1) sudokus) of
            Nothing ->
                [ 0, 0, 0, 0, 0, 0, 0, 1, 3, 0, 4, 0, 0, 0, 0, 0, 8, 0, 2, 0, 0, 0, 6, 0, 0, 0, 0, 6, 0, 9, 0, 0, 0, 4, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 3, 0, 1, 0, 0, 5, 0, 0, 0, 0, 0, 0, 4, 0, 7, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]

            Just x ->
                x



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
        case foldr foldfunction 1 x of
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


berechneTipp : Sudokufeld -> Pos -> Maybe ( Pos, Int )



-- Schlägt fehl wenn kein einfach eindeutiges Feld forliegt, momentan mehr wie eine auto vervollständigung.


berechneTipp feld startpos =
    if fst startpos < 10 && snd startpos < 10 then
        let
            ziffernliste =
                moeglicheZiffern feld startpos
        in
          case getField feld startpos of

              Nothing ->
                  case length ziffernliste of
                      0 -> -- Wenn ein Feld nicht belegt werden kann, und dieses ist nicht bereits gefüllt, ist das Spiel so nicht lösbar
                          Nothing
                      1 ->
                          Just
                              ( startpos
                              , case head ziffernliste of
                                  Just x ->
                                      x

                                  Nothing -> --Da length 1 zurückgab, kann dies nicht passieren, aber benötigt um head zu benutzen.
                                      0
                              )

                      -- Rückgabe des eindeutig lösbaren Felds und der entsprechende Eintrag, der Nothing-Fall tritt nie ein
                      _ ->
                          case fst startpos of
                              -- Mehrere Einträge gültig, iteriere über das Feld
                              9 ->
                                  berechneTipp feld ( 1, snd startpos + 1 )

                              _ ->
                                  berechneTipp feld ( fst startpos + 1, snd startpos )

              Just _ ->
                  case fst startpos of
                      -- Einzelnes Feld gefüllt, iteriere über das Sudokufeld
                      9 ->
                          berechneTipp feld ( 1, snd startpos + 1 )

                      _ ->
                          berechneTipp feld ( fst startpos + 1, snd startpos )
    else
        Nothing



-- Das gesamte Feld wurde untersucht, aber es gibt keine eindeutig lösbaren Felder
-- Berechnet eine Position und einen Wert für einen eindeutig lösbaren Eintrag

berechneLoesung : Sudokufeld -> Maybe Sudokufeld
berechneLoesung feld = loeseFeld feld (1, 1)
--Ausgabefunktion für die Lösung eines eingegenen Feldes

loeseFeld : Sudokufeld -> Pos -> Maybe Sudokufeld
loeseFeld feld pos = case getField feld pos of --Überprüfung, ob das Feld gefüllt ist
                          Nothing -> Nothing --Hier kommt die eigentliche Logik rein
                          Just _ ->
                                case fst pos of
                                --Das Feld ist bereits eingetragen und soll nicht bearbeitet werden, gehe zum nächsten Feld
                                    9 -> loeseFeld feld (1, snd pos +1)
                                    _ -> loeseFeld feld (fst pos +1, snd pos)