module Operationen exposing (..)

import Array exposing (..)
import List


type alias Pos =
    ( Int, Int )


type alias Sudokufeld =
    Array (Array ( Maybe Int, Bool ))


getField : Sudokufeld -> Pos -> Maybe Int
getField feld pos =
    case get (fst pos - 1) feld of
        Nothing ->
            Nothing

        Just row ->
            case get (snd pos - 1) row of
                Nothing ->
                    Nothing

                Just feld ->
                    fst feld


setField : Sudokufeld -> Pos -> Int -> Sudokufeld
setField feld pos zahl =
    let
        modzahl =
            if zahl == 0 then
                Nothing
            else
                Just zahl
    in
        case isProtected feld pos of
            False ->
                let
                    row =
                        case get (fst pos - 1) feld of
                            Just row' ->
                                set (snd pos - 1) ( modzahl, False ) row'

                            Nothing ->
                                empty
                in
                    set (fst pos - 1) row feld

            True ->
                feld


getColumn : Sudokufeld -> Int -> List (Maybe Int)
getColumn feld spalte =
    let
        specialGet =
            \x y ->
                case get (x - 1) y of
                    Nothing ->
                        ( Just 0, False )

                    Just smth ->
                        smth
    in
        List.map fst (toList (map (specialGet spalte) feld))


getRow : Sudokufeld -> Int -> List (Maybe Int)
getRow feld zeile =
    let
        zeilenbool =
            case get (zeile - 1) feld of
                Nothing ->
                    []

                Just zeilenbool' ->
                    toList zeilenbool'
    in
        List.map fst zeilenbool


getSubsquare : Sudokufeld -> Int -> List (Maybe Int)
getSubsquare feld unterfeld =
    let
        row =
            List.map ((+) (((unterfeld - 1) // 3) * 3)) [ 1, 2, 3, 1, 2, 3, 1, 2, 3 ]

        column =
            List.map ((+) (((unterfeld - 1) % 3) * 3)) [ 1, 1, 1, 2, 2, 2, 3, 3, 3 ]
    in
        let
            poses =
                List.map2 (,) row column
        in
            List.map (getField feld) poses


setProtected : Sudokufeld -> Pos -> Sudokufeld
setProtected feld pos =
    let
        row =
            case get (fst pos - 1) feld of
                Just row' ->
                    set (snd pos - 1) ( getField feld pos, True ) row'

                Nothing ->
                    empty
    in
        set (fst pos - 1) row feld


isProtected : Sudokufeld -> Pos -> Bool
isProtected feld pos =
    case get (fst pos - 1) feld of
        Nothing ->
            False

        Just row ->
            case get (snd pos - 1) row of
                Nothing ->
                    False

                Just intbool ->
                    snd intbool


circularnext : Int -> Int
circularnext x = case x of
                  9->1
                  _->x + 1

circularprev : Int -> Int
circularprev x = case x of
                  1->9
                  _->x - 1

emptyField : Sudokufeld
emptyField =
    Array.repeat 9 (Array.repeat 9 ( Nothing, False ))



-- Erstellt ein leeres Feld, bei dem kein Schreibschutz aktiviert ist
