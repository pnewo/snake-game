module Types.Model exposing (..)

import Types.Snake exposing (Snake)
import Types.GameArea exposing (Location)


type alias Model =
    { snake : Snake
    , foodLocation : Location
    , paused : Bool
    , turnBuffer : List Int
    , score : Int
    , topScores : List Int
    }
