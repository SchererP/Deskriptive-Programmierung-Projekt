module Operationen exposing (..)

type alias Pos = (int, int)
type alias Sudokufeld = Array (Array (Maybe int, Bool))

getField : Sudokufeld -> Pos -> Maybe int
getField = 

setField : Sudokufeld -> Pos -> int -> Sudokufeld
setField = 

getColumn : Sudokufeld -> int -> List (Maybe int)
getColumn = 

getRow : Sudokufeld -> int -> List (Maybe int)
getRow = 

getSubsquare : Sudokufeld -> int -> List (Maybe int)
getSubsquare = 

setProtected : Sudokufeld -> Pos -> Sudokufeld
setProtected = 

emptyField : Sudokufeld
emptyField = Array.repeat 9 (Array.repeat 9 (Nothing, False))
-- Erstellt ein leeres Feld, bei dem kein Schreibschutz aktiviert ist

toHMTL : Sudokufeld -> HTML
toHTML = 
