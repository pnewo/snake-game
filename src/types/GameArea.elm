module Types.GameArea exposing (..)


gameAreaSize : Int
gameAreaSize =
    10


type Square
    = SnakeHead Direction
    | SnakeTail
    | Food
    | Empty


type Direction
    = Up
    | Right
    | Down
    | Left


directionToInt : Direction -> Int
directionToInt direction =
    case direction of
        Up ->
            0

        Right ->
            1

        Down ->
            2

        Left ->
            3


intToDirection : Int -> Direction
intToDirection int =
    case int of
        (-1) ->
            Left

        0 ->
            Up

        1 ->
            Right

        2 ->
            Down

        3 ->
            Left

        _ ->
            Up


turnDirection : Int -> Direction -> Direction
turnDirection turn direction =
    direction
        |> directionToInt
        |> (\dirInt -> dirInt + turn)
        |> intToDirection


hasCrashed : Location -> List Location -> Bool
hasCrashed ( row, col ) tailLocations =
    row
        < 0
        || row
        >= gameAreaSize
        || col
        < 0
        || col
        >= gameAreaSize
        || List.member ( row, col ) tailLocations


type alias Location =
    ( Int, Int )


newFoodLocation : List Location -> Location
newFoodLocation snakeLocationList =
    ( 0, 0 )


type alias GameAreaRow =
    List Square


type alias GameArea =
    List GameAreaRow
