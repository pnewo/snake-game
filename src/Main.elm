module Main exposing (..)

import Time exposing (Time, millisecond)
import Keyboard exposing (KeyCode)
import Random exposing (generate, int)
import Set
import Array
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Components.GameArea exposing (..)
import Types.Messages exposing (..)
import Types.Model exposing (..)
import Types.GameArea exposing (..)


---- MODEL ----


initSnakeLocation : Location
initSnakeLocation =
    ( 0, 0 )


initSnakeTailLength : Int
initSnakeTailLength =
    1


storedScoresAmount : Int
storedScoresAmount =
    5


initModel : Model
initModel =
    { snake =
        { head =
            { direction = Right
            , location = initSnakeLocation
            }
        , tail = List.repeat initSnakeTailLength initSnakeLocation
        }
    , foodLocation = ( 0, 1 )
    , paused = True
    , turnBuffer = []
    , score = 0
    , topScores = List.repeat storedScoresAmount 0
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , newFoodCommands [ initSnakeLocation ]
    )



---- UPDATE ----


getNewHeadLocation : Direction -> Location -> Location
getNewHeadLocation direction location =
    let
        sum : Int -> Int -> Int
        sum int1 int2 =
            int1 + int2

        add =
            sum 1

        sub =
            sum -1
    in
        case direction of
            Up ->
                Tuple.mapFirst sub location

            Right ->
                Tuple.mapSecond add location

            Down ->
                Tuple.mapFirst add location

            Left ->
                Tuple.mapSecond sub location


getTopScores : List Int -> Int -> List Int
getTopScores topScores newScore =
    newScore
        :: topScores
        |> List.sort
        |> List.reverse
        |> List.take storedScoresAmount


newFoodCommands : List Location -> Cmd Msg
newFoodCommands snakeLocations =
    let
        randomMax =
            gameAreaSize * gameAreaSize - 1 - (List.length snakeLocations)
    in
        Random.generate NewFood (Random.int 0 randomMax)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                snakeHead =
                    model.snake.head

                ( newDirection, newTurnBuffer ) =
                    case model.turnBuffer of
                        turn :: rest ->
                            ( turnDirection turn model.snake.head.direction, rest )

                        _ ->
                            ( model.snake.head.direction, model.turnBuffer )

                newLocation =
                    getNewHeadLocation newDirection model.snake.head.location

                newHead =
                    { snakeHead
                        | location = newLocation
                        , direction = newDirection
                    }

                isFoodEaten =
                    newLocation == model.foodLocation

                ( moveTail, newScore ) =
                    if isFoodEaten then
                        ( 0, model.score + 1 )
                    else
                        ( 1, model.score )

                newTail =
                    model.snake.head.location
                        :: (model.snake.tail
                                |> List.take (List.length model.snake.tail - moveTail)
                           )

                foodCommand =
                    if isFoodEaten then
                        newFoodCommands (newLocation :: newTail)
                    else
                        Cmd.none

                newSnake =
                    { head = newHead
                    , tail = newTail
                    }

                crashed =
                    hasCrashed newLocation newTail
            in
                if crashed then
                    ( { initModel | topScores = getTopScores model.topScores newScore }, newFoodCommands [ initSnakeLocation ] )
                else
                    ( { model
                        | snake = newSnake
                        , turnBuffer = newTurnBuffer
                        , score = newScore
                      }
                    , foodCommand
                    )

        TurnPress turn ->
            let
                newTurnBuffer =
                    if List.length model.turnBuffer < 2 then
                        List.append model.turnBuffer [ turn ]
                    else
                        model.turnBuffer
            in
                ( { model | turnBuffer = newTurnBuffer }, Cmd.none )

        NewFood randomAreaInt ->
            let
                snakeLocationSet =
                    model.snake.head.location
                        :: model.snake.tail
                        |> Set.fromList

                newLocation =
                    generateEmptyGameArea
                        |> List.indexedMap
                            (\rowIndex areaRow ->
                                List.indexedMap
                                    (\colIndex _ ->
                                        ( rowIndex, colIndex )
                                    )
                                    areaRow
                            )
                        |> List.concat
                        |> Set.fromList
                        |> flip Set.diff snakeLocationSet
                        |> Set.toList
                        |> Array.fromList
                        |> Array.get randomAreaInt
                        |> Maybe.withDefault ( -1, -1 )
            in
                if Tuple.first newLocation < 0 then
                    ( { model | paused = True }, Cmd.none )
                else
                    ( { model | foodLocation = newLocation }, Cmd.none )

        Paused ->
            ( { model | paused = not model.paused }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg", onClick Paused ] []
        , h1 [] [ text "Snake" ]
        , renderGameArea model
        , div [] [ text <| toString model.score ]
        , renderTopScores model.topScores
        ]


renderTopScores : List Int -> Html Msg
renderTopScores intList =
    div []
        (intList
            |> List.indexedMap (\index score -> div [] [ text <| (toString (index + 1) ++ ". " ++ toString score) ])
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subArr =
            if model.paused then
                [ Sub.none ]
            else
                [ Time.every (250 * millisecond) Tick
                , Keyboard.downs key
                ]
    in
        Sub.batch subArr


key : KeyCode -> Msg
key keycode =
    case keycode of
        37 ->
            TurnPress -1

        39 ->
            TurnPress 1

        _ ->
            NoOp



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
