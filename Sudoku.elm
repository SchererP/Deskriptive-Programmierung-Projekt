module Sukoku exposing (..)

import Logik exposing (..)
import Operationen exposing (..)

import Html exposing (..)
import Mouse

type Msg = 

init = ({spielfeld = , modus = True, markiert = (0, 0)}, Cmd.none)

main =
    programm
    {
      init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
    
type alias model = { spielfeld : Sudokufeld, modus : Bool, markiert : Pos }
-- spielfeld speichert das Array mit Typ Maybe int, Bool; Maybe int legt den Inhalt fest und Bool speichert, ob es sich um einen Startwert handelt
-- modus legt fest, ob die möglichen Einträge angezeigt werden oder nicht
-- markiert speichert die Koordinaten des aktuell ausgewählten Feldes

view {spielfeld, modus, markiert} =
    div[]
    [
        header [] [text "Sudoku"] --Anpassung des Headers im Browser
      , h1 [] [text "Sudokuspiel"] --Titel des Spiels
      , table [] [text "Modus", select [] [text "Mit Hilfestellung", text "Ohne Hilfestellung"]] --Menü zur Modusauswahl
      , table [] [tr [] [td [] [text "1"], td [] [text "2"], td [] [text "3"], td [] [text "4"], td [] [text "5"], td [] [text "6"], td [] [text "7"], td [] [text "8"], td [] [text "9"]]] --Auswahlfeld der einzufügenden Ziffer
      , h2 [] [text "Programmerklärungen:"] --Hinweistexte für die auszuwählenden Menüs
      , h3 [] [text "Modus"]
      , p [] [text "Das Spiel bietet zwei Modi an: mit Hilfestellung oder ohne. Mit Hilfestellung werden alle für das ausgewählte Feld ungültigen Ziffern ausgegraut und ohne Hilfestellung alle Ziffern immer zur Auswahl gestellt."]
      , footer [] [text "Eine Implementierung von Jonas Barteldrees und Paul Scherer", text "Projekt im Rahmen der Vorlesung Deskriptive Programmierung"] --Fußzeile mit Autorenangabe
    ]
    
update msg ({spielfeld, modus, markiert} as model) =
    case msg of