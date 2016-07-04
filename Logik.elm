module Logik exposing (..)

import Operationen exposing (..)

generiereFeld : int -> Sudokufeld
generiereFeld = 
-- Generiert ein eindeutig-lösbares Feld und entfernt ableitbare Einträge
-- Der int-Wert gibt den Schwierigkeitsgrad an

ueberpruefeFeld : Sudokufeld -> Maybe Bool
ueberpruefeFeld = 
-- Kontrolliert, ob das Feld vollständig gefüllt und korrekt belegt wurde. Für nicht-vollständige Felder wird Nothing zurückgegeben und sonst Bool

moeglicheZiffern : Sudokufeld -> Pos -> List int
moeglicheZiffern = 
-- Liefert eine Liste mit möglichen Einträgen
