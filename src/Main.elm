module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing(Array)
import Array
import Browser
import List exposing (range)
import Html exposing (Html, button, div, h1, h2, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


main =
  Browser.sandbox { init = init, update = update, view = view }



--Model


type alias Model =
  { boardState : BoardState
  , gameStatus : GameStatus
  , currentPlayer : Player 
  }

type SquareValue
  = X
  | O
  | Empty

type Player 
  = PlayerX 
  | PlayerO

type alias BoardState =
  Array SquareValue

type Msg
  = Reset
  | Mark Int

type GameStatus
  = XWins
  | OWins
  | Nobody


cleanBoard : Array SquareValue
cleanBoard = Array.initialize 9 (always Empty)


init : Model
init =
  { boardState = cleanBoard 
  , gameStatus = Nobody 
  , currentPlayer = PlayerX }


playerMark : Player -> SquareValue
playerMark player =
  case player of
    PlayerX -> X
    PlayerO -> O


nextPlayer : Player -> Player
nextPlayer player =
  case player of
    PlayerX -> PlayerO
    PlayerO -> PlayerX


playTurn : Model -> Int -> Model
playTurn model index =
  let
    newBoardA = Array.set index (playerMark model.currentPlayer) model.boardState
    gameStatus = checkGameStatus newBoardA winCombos
    newBoardB =
      case gameStatus of
        XWins -> 
          Array.initialize 9 (always X)
        OWins -> 
          Array.initialize 9 (always O)
        Nobody -> 
          newBoardA
  in
    { model
    | gameStatus = gameStatus
    , boardState = newBoardB
    , currentPlayer = (nextPlayer model.currentPlayer)
    }

viewBoardRow : BoardState -> Int -> Html Msg
viewBoardRow board index =
  let 
    rangeStart = (index - 1) * 3 
    rangeEnd = index * 3 - 1
  in 
    div [ class "row" ] [ 
      div [] (range rangeStart rangeEnd |> List.map (viewBoardButton board))
    ]

viewBoardButton : BoardState -> Int -> Html Msg
viewBoardButton board index =
  button [ class "button", style "background-color" "red", onClick (Mark index) ] [ Array.get index board |> viewBoardButtonText |> text ]


viewBoardButtonText : Maybe SquareValue -> String
viewBoardButtonText maybeSquare =
  case maybeSquare of
    Just squareValue ->
      case squareValue of
        X -> "X"
        O -> "O"
        Empty -> "-"
    Nothing ->
      ""

winCombos : List (Int, Int, Int)
winCombos =
  [
    (0, 1, 2),
    (3, 4, 5),
    (6, 7, 8),
    (0, 3, 6),
    (1, 4, 7),
    (2, 5, 8),
    (0, 4, 8),
    (2, 4, 6)
  ]

checkGameStatus : BoardState -> List (Int,Int,Int) -> GameStatus
checkGameStatus board uncheckedWinCombos =
  case uncheckedWinCombos of 
    [] -> 
      Nobody

    head::tail -> 
      let 
          (a,b,c) = head

      in
          if (Array.get a board) == Just X && (Array.get b board)  == Just X && (Array.get c board) ==Just X then
            XWins

          else if (Array.get a board) == Just O && (Array.get b board)  == Just O && (Array.get c board) == Just O then
            OWins

          else 
            checkGameStatus board tail

viewGameStatusText : GameStatus -> String 
viewGameStatusText gameStatus =
  case gameStatus of 
    Nobody -> 
      "Game In Progress"

    XWins -> 
      "X Wins!!!"

    OWins -> 
      "O Wins!!!"


--Update

update : Msg -> Model -> Model
update msg model =
  case msg of
    Reset ->
      { model 
      | boardState = cleanBoard 
      , gameStatus = Nobody 
      , currentPlayer = PlayerX }

    Mark index ->
      playTurn model index


--View

view : Model -> Html Msg
view model =
  div [ class "main" ]
    [ div []
      [ h1 [] [ text "Tic Tac Toe" ]
      , h2 [ class "game-status" ] [ text (viewGameStatusText model.gameStatus) ]
      , button [ onClick Reset] [ text "Reset" ]
      ]
    , div [] (range 1 3 |> List.map (viewBoardRow model.boardState))
    ]
