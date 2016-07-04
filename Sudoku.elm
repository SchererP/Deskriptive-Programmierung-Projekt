module Sukoku exposing (..)

import Logik exposing (..)
import Operationen exposing (..)

main =
    programm
    {
    init = 
    , view = 
    , update = 
    , subscriptions = subscriptions
    }
    
type alias model = { spielfeld : Sudokufeld, modus : Bool, markiert : Pos }
-- spielfeld speichert das Array mit Typ Maybe int, Bool; Maybe int legt den Inhalt fest und Bool speichert, ob es sich um einen Startwert handelt
-- modus legt fest, ob die möglichen Einträge angezeigt werden oder nicht
-- markiert speichert die Koordinaten des aktuell ausgewählten Feldes
