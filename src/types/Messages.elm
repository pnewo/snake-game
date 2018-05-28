module Types.Messages exposing (..)

import Time exposing (Time)


type Msg
    = Tick Time
    | TurnPress Int
    | NewFood Int
    | Paused
    | NoOp
