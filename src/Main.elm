module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing(Array)
import Array
import Browser
import List exposing (range)
import Html exposing (Html, button, div, h1, h2, text, h4)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


main =
  Browser.document { init = init, update = update, view = view , subscriptions = \_ -> Sub.none}

--Model

type alias Document msg =
  { title : String
  , body : List (Html msg)
  }

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
  | Tie 


init : () -> ( Model, Cmd Msg )
init _ = 
  ( { boardState = cleanBoard 
  , gameStatus = Nobody 
  , currentPlayer = PlayerX }, Cmd.none)


cleanBoard : Array SquareValue
cleanBoard = Array.initialize 9 (always Empty)



playerMark : Player -> SquareValue
playerMark player =
  case player of
    PlayerX -> X
    PlayerO -> O


changePlayer : Player -> Player
changePlayer player =
  case player of
    PlayerX -> PlayerO
    PlayerO -> PlayerX


playTurn : Model -> Int -> Model
playTurn model index =
  let
    newBoardA = 
      case (Array.get index model.boardState) of
        Just squareValue -> 
          case squareValue of
            X -> 
              model.boardState
            O -> 
              model.boardState
            Empty ->  
              Array.set index (playerMark model.currentPlayer) model.boardState
        Nothing ->
          model.boardState

    nextPlayer = 
      if model.boardState == newBoardA then 
        model.currentPlayer
      else 
        changePlayer model.currentPlayer
    
    gameStatus = checkGameStatus newBoardA winCombos
    newBoardB =
      case gameStatus of
        XWins -> 
          Array.initialize 9 (always X)
        OWins -> 
          Array.initialize 9 (always O)
        Tie ->
          Array.initialize 9 (always Empty)
        Nobody -> 
          newBoardA
  in
    { model
    | gameStatus = gameStatus
    , boardState = newBoardB
    , currentPlayer = nextPlayer
    }

isSquareOccupied : SquareValue -> Bool
isSquareOccupied state =
  case state of
    X -> 
      True
    O ->
      True
    Empty ->
      False


viewBoardRow : BoardState -> Int -> Html Msg
viewBoardRow board index =
  let 
    rangeStart = (index - 1) * 3 
    rangeEnd = index * 3 - 1
  in 
    div [ class "row" ] (range rangeStart rangeEnd |> List.map (viewBoardButton board))


viewBoardButton : BoardState -> Int -> Html Msg
viewBoardButton board index =
  button [ class ("square square-" ++ (String.fromInt index)), style "background-color" "red", onClick (Mark index) ] [ Array.get index board |> viewBoardButtonText |> text ]


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

          else if Array.length (Array.filter isSquareOccupied board) == Array.length board then
            Tie
          
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

    Tie ->
      "No Winners :("

--Update

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      ( { model 
      | boardState = cleanBoard 
      , gameStatus = Nobody 
      , currentPlayer = PlayerX } , Cmd.none )

    Mark index ->
      ( (playTurn model index), Cmd.none )


--View

view : Model -> Document Msg
view model =
  { title = "Elm-Tac-Toe"
  , body = [ div [ class "main" ]
    [ div [ class "game-header" ]
      [ h1 [ class "game-header-title"] [ text "Elm-Tac-Toe" ]
      , h4 [ class "game-header-status-title" ] [ text "Status" ] 
      , h2 [ class "game-header-status-content" ] [ text (viewGameStatusText model.gameStatus) ]
      , button [ onClick Reset] [ text "Reset" ]
      ]
    , div [ class "game-content" ] (range 1 3 |> List.map (viewBoardRow model.boardState))
    ] ]}
