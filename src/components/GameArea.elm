module Components.GameArea exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (class)
import List exposing (indexedMap, repeat)
import Types.Messages exposing (Msg)
import Types.Model exposing (Model)
import Types.GameArea exposing (..)


generateEmptyGameArea : List (List Square)
generateEmptyGameArea =
    repeat gameAreaSize (repeat gameAreaSize Empty)


renderGameArea : Model -> Html Msg
renderGameArea model =
    let
        snakeHeadLocation =
            model.snake.head.location

        snakeHeadDirection =
            model.snake.head.direction

        snakeTail =
            model.snake.tail

        gameArea : GameArea
        gameArea =
            generateEmptyGameArea
                |> indexedMap
                    (\rowIndex areaRow ->
                        indexedMap
                            (\colIndex square ->
                                if ( rowIndex, colIndex ) == snakeHeadLocation then
                                    SnakeHead snakeHeadDirection
                                else if (List.member ( rowIndex, colIndex ) snakeTail) then
                                    SnakeTail
                                else if ( rowIndex, colIndex ) == model.foodLocation then
                                    Food
                                else
                                    square
                            )
                            areaRow
                    )
    in
        div [ class "game-area" ] (indexedMap (\index gameAreaRow -> div [ class "game-area__row" ] (renderGameAreaRow gameAreaRow index)) gameArea)


renderGameAreaRow : GameAreaRow -> Int -> List (Html Msg)
renderGameAreaRow gameAreaRow rowIndex =
    indexedMap
        (\index square ->
            case square of
                SnakeHead direction ->
                    div [ class "game-area__cell--snake-head" ] [ text <| renderSnakeHead direction ]

                SnakeTail ->
                    div [ class "game-area__cell--snake-tail" ] [ text "" ]

                Food ->
                    div [ class "game-area__cell--food" ] [ text "" ]

                Empty ->
                    div [ class "game-area__cell" ] [ text "" ]
        )
        gameAreaRow


renderSnakeHead : Direction -> String
renderSnakeHead direction =
    case direction of
        Up ->
            ""

        Right ->
            ""

        Down ->
            ""

        Left ->
            ""
