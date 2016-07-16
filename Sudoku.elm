module Sukoku exposing (..)

import Logik exposing (..)
import Operationen exposing (..)
import Html exposing (..)
import Html.App exposing (program)


--import TimeTravel.Html.App exposing (program)

import Html.Events exposing (onClick, onCheck)
import List exposing (map)


type Msg
    = Moduswechsel Bool
    | Feldauswahl Pos
    | Zifferneingabe Int
    | Abschluss


init =
    ( { spielfeld = generiereFeld 1, modus = True, markiert = ( 1, 1 ), abschluss = Nothing }, Cmd.none )


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { spielfeld : Sudokufeld, modus : Bool, markiert : Pos, abschluss : Maybe Bool }



-- spielfeld speichert das Array mit Typ Maybe int, Bool; Maybe int legt den Inhalt fest und Bool speichert, ob es sich um einen Startwert handelt
-- modus legt fest, ob die möglichen Einträge angezeigt werden oder nicht
-- markiert speichert die Koordinaten des aktuell ausgewählten Feldes


view model =
    div []
        [ header [] [ text "Sudoku" ]
          --Anpassung des Headers im Browser
        , h1 [] [ text "Sudokuspiel" ]
          --Titel des Spiels
        , table [] [ tr [] [ td [] [ text "Modus" ] ], tr [] [ td [] [ select [] [ option [ onClick (Moduswechsel True) ] [ text "Mit Hilfestellung" ], option [ onClick (Moduswechsel False) ] [ text "Ohne Hilfestellung" ] ] ] ] ]
          --Menü zur Modusauswahl
        , druckeSpielfeld model.spielfeld
          --Ausgabe des Spielfelds
        , druckeZahlenfeld model.spielfeld model.markiert model.modus
          --Auswahlfeld der einzufügenden Ziffer
        , button [ onClick Abschluss ] [ text "Abschlusskontrolle" ]
          --Anstoß der Kontrollroutine
        , case model.abschluss of
            --Ausgabe des Kontrollergebnisses
            Nothing ->
                text "Das Feld war bei der letzten Kontrolle nicht vollständig gefüllt."

            Just True ->
                text "Herzlichen Glückwunsch! Sie haben das Spiel erfolgreich gelöst."

            Just False ->
                text "Das Feld war bei der letzten Kontrolle leider nicht korrekt gefüllt."
        , h2 [] [ text "Programmerklärungen:" ]
          --Hinweistexte für die auszuwählenden Menüs
        , h3 [] [ text "Modus" ]
        , p [] [ text "Das Spiel bietet zwei Modi an: mit Hilfestellung oder ohne. Mit Hilfestellung werden alle für das ausgewählte Feld ungültigen Ziffern ausgegraut und ohne Hilfestellung alle Ziffern immer zur Auswahl gestellt." ]
        , footer [] [ text "Eine Implementierung von Jonas Barteldrees und Paul Scherer", text "Projekt im Rahmen der Vorlesung Deskriptive Programmierung" ]
          --Fußzeile mit Autorenangabe
        ]


druckeZahlenfeld : Sudokufeld -> Pos -> Bool -> Html Msg
druckeZahlenfeld feld pos modus =
    let
        toButton =
            \x -> td [] [ button [ onClick (Zifferneingabe x) ] [ text (toString x) ] ]
    in
        table []
            [ tr []
                ((td [] [ button [ onClick (Zifferneingabe 0) ] [ text "Löschen" ] ])
                    :: (map toButton
                            (if modus then
                                moeglicheZiffern feld pos
                             else
                                [1..9]
                            )
                       )
                )
            ]


druckeSpielfeld : Sudokufeld -> Html Msg
druckeSpielfeld feld =
    let
        toButton =
            \pos -> td [] [ button [ onClick (Feldauswahl pos) ] [ text (toString (getField feld pos)) ] ]
    in
        let
            toButtonRow =
                \x -> tr [] (map ((curry toButton) x) [1..9])
        in
            table [] (map toButtonRow [1..9])


update msg model =
    case msg of
        Feldauswahl pos ->
            ( { model | markiert = pos }, Cmd.none )

        Zifferneingabe i ->
            ( { model | spielfeld = setField model.spielfeld model.markiert i }, Cmd.none )

        Abschluss ->
            ( { model | abschluss = ueberpruefeFeld model.spielfeld }, Cmd.none )

        Moduswechsel b ->
            ( { model | modus = b }, Cmd.none )


subscriptions model =
    Sub.none
