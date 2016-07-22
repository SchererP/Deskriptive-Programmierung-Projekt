module Sukoku exposing (..)

import Logik exposing (..)
import Operationen exposing (..)
import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (style)
import Time exposing (Time, second)
import Keyboard

--import TimeTravel.Html.App exposing (program)

import Html.Events exposing (onClick, onCheck)
import List exposing (map)
import Random exposing (int, generate)

type Msg
    = Moduswechsel Bool
    | Feldauswahl Pos
    | Zifferneingabe Int
    | Abschluss
    | Tipp
    | NeuesFeld
    | WaehleFeld Int
    | SetzeAutofill (Maybe (Pos, Int))

init =
    ( { spielfeld = generiereFeld 1, modus = True, markiert = ( 1, 1 ), abschluss = Nothing, autofill = Nothing }, Cmd.none )


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { spielfeld : Sudokufeld, modus : Bool, markiert : Pos, abschluss : Maybe Bool, autofill : Maybe Bool}



-- spielfeld speichert das Array mit Typ Maybe int, Bool; Maybe int legt den Inhalt fest und Bool speichert, ob es sich um einen Startwert handelt
-- modus legt fest, ob die möglichen Einträge angezeigt werden oder nicht
-- markiert speichert die Koordinaten des aktuell ausgewählten Feldes


view model =
    div []
        [ --header [] [ text "Sudoku" ]
          --Anpassung des Headers im Browser
         h1 [] [ text "Sudokuspiel" ]
          --Titel des Spiels
        , table [] [ tr [] [ td [] [ text "Modus:" ] ], tr [] [ td [] [ select [] [ option [ onClick (Moduswechsel True) ] [ text "Mit Hilfestellung" ], option [ onClick (Moduswechsel False) ] [ text "Ohne Hilfestellung" ] ] ], td [] [button [ onClick NeuesFeld ] [ text "Neues Feld" ]] ] ]
          --Menü zur Modusauswahl
        , br [] [text "Warum verschwindet dieser Text?"]
        , druckeSpielfeld model.spielfeld model.markiert
          --Ausgabe des Spielfelds
        , br [] [text "Warum verschwindet dieser Text?"]
        , druckeZahlenfeld model.spielfeld model.markiert model.modus
          --Auswahlfeld der einzufügenden Ziffer
        , br [] [text "Warum verschwindet dieser Text?"]
        , text (toString model.debugint)
        , button [ onClick Abschluss ] [ text "Abschlusskontrolle" ]
          --Anstoß der Kontrollroutine
        , button [ onClick Tipp ] [ text "Autofüllen" ]
        , br [] [text "Warum verschwindet dieser Text?"]
        , br [] [text "Warum verschwindet dieser Text?"] --Dummyelement, um einen Zeilenumbruch zu realisieren
        , case model.abschluss of
            --Ausgabe des Kontrollergebnisses
            Nothing ->
                text "Das Feld war bei der letzten Kontrolle nicht vollständig gefüllt."

            Just True ->
                text "Herzlichen Glückwunsch! Sie haben das Spiel erfolgreich gelöst."

            Just False ->
                text "Das Feld war bei der letzten Kontrolle leider nicht korrekt gefüllt."
        , br [] [text "Warum verschwindet dieser Text?"]
        , case model.autofill of
            Nothing ->
              text "Autofill könnte möglich sein."
            Just True ->
              text "Es wurde ein Feld automatisch gefüllt."
            Just False ->
              text "Es wurde kein automatisch füllbares Feld gefunden."
        , h2 [] [ text "Programmerklärungen:" ]
          --Hinweistexte für die auszuwählenden Menüs
        , h3 [] [ text "Modus" ]
        , p [] [ text "Das Spiel bietet zwei Modi an: mit Hilfestellung oder ohne. Mit Hilfestellung werden alle für das ausgewählte Feld ungültigen Ziffern ausgeblendet und ohne Hilfestellung alle Ziffern immer zur Auswahl gestellt." ]
        , h3 [] [text "Autofüllen" ]
        , p [] [ text "Mit 'Autofüllen' wird ein eindeutig lösbares Feld angezeigt, sofern der aktuelle Zustand des Feldes nicht wegen falscher Belegung(en) unlösbar ist. Wird kein Eintrag verändert, so ist kein direkt eindeutiges Feld vorhanden oder das Spielfeld ist nicht mehr lösbar." ]
        , br [] [text "Warum verschwindet dieser Text?"]
        , br [] [text "Warum verschwindet dieser Text?"]
        , footer [] [ text "Ein Projekt von Jonas Barteldrees und Paul Scherer im Rahmen der Vorlesung Deskriptive Programmierung im Sommersemester 2016"]
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


druckeSpielfeld : Sudokufeld -> Pos -> Html Msg
druckeSpielfeld feld markiert =
    let
        toButton =
            \pos -> td [ style [if pos == markiert then ("backgroundColor", "blue") else ("backgroundColor", "white"), ("border", "50"), ("borderColor", "black")]] [ button [ onClick (Feldauswahl pos)] [ druckeZahl feld pos ]]
        toButtonRow =
            \x -> tr [] (map ((curry toButton) x) [1..9])
        in
            table [] (map toButtonRow [1..9])

druckeZahl : Sudokufeld -> Pos -> Html Msg
druckeZahl feld pos = case getField feld pos of
                            Just i -> if (isProtected feld pos) then strong [] [text (toString i)] else text (toString i)
                            Nothing -> text "_"

zufaelligesFeld : Int->Msg
zufaelligesFeld feldint=
  WaehleFeld feldint

keycodeToMsg : Model -> Keyboard.KeyCode -> Msg
--37 links, 38 hoch, 39 rechts, 40 unten
keycodeToMsg model code = case code of
                    37 -> Feldauswahl (fst model.markiert, circularprev(snd model.markiert))
                    38 -> Feldauswahl (circularprev(fst model.markiert), snd model.markiert)
                    39 -> Feldauswahl (fst model.markiert, circularnext(snd model.markiert))
                    40 -> Feldauswahl (circularnext(fst model.markiert), snd model.markiert)
                    _ -> Abschluss --Platzhalter


update msg model =
    case msg of
        Feldauswahl pos ->
            ( { model | markiert = pos }, Cmd.none )

        Zifferneingabe i ->
            ( { model | spielfeld = setField model.spielfeld model.markiert i, autofill = if (isProtected model.spielfeld model.markiert) then model.autofill else Nothing }, Cmd.none )

        Abschluss ->
            ( { model | abschluss = ueberpruefeFeld model.spielfeld }, Cmd.none )

        Moduswechsel b ->
            ( { model | modus = b }, Cmd.none )
        Tipp ->
            (model,generate (\_->SetzeAutofill (berechneTipp model.spielfeld (1, 1))) (int 0 0))
        SetzeAutofill tipp ->
            let
              newfield = case tipp of
                          Just (pos, i) -> setField model.spielfeld pos i
                          Nothing -> model.spielfeld
          in
            ({model | spielfeld = newfield, autofill = if tipp == Nothing then Just False else Just True}, Cmd.none)
        NeuesFeld ->
          ( model, generate zufaelligesFeld (int 1 20))

        WaehleFeld i ->
          ({model|spielfeld = generiereFeld i}, Cmd.none)



subscriptions model =
    Sub.batch
    [
          Time.every second (\_ -> Abschluss)
        , Keyboard.presses (keycodeToMsg model)
    ]
