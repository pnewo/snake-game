module Types.Snake exposing (..)

import Types.GameArea exposing (Location, Direction)


type alias Snake =
    { head :
        SnakeHead
    , tail : SnakeTail
    }


type alias SnakeHead =
    { direction : Direction
    , location : Location
    }


type alias SnakeTail =
    List Location
