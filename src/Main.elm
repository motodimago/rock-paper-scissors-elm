import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random

-- MAIN

main =
    Browser.document
    { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
    }

-- MODEL
type Hand = Rock | Paper | Scissors
type GameResult = Win | Lose | Draw | None

hand : Random.Generator Hand
hand = Random.uniform Rock [ Paper, Scissors ]

type alias Model = 
  { mine: Hand
  , enemy: Hand
  , result: GameResult
  }

init : () -> (Model, Cmd Msg)
init _ =
    (Model Rock Rock None, Cmd.none)


-- UPDATE
type Msg = Next Hand | NewEnemyHand Hand

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Next myHand ->
      ( { model | mine = myHand }
      , Random.generate NewEnemyHand hand
      )
    NewEnemyHand newEnemyHand ->
      ( Model model.mine newEnemyHand (result model.mine newEnemyHand)
      , Cmd.none
      )      

result : Hand -> Hand -> GameResult
result mine enemy = 
  if (mine == Rock) then
    if (enemy == Paper) then
      Lose
    else if (enemy == Scissors) then
      Win
    else
      Draw
  else if (mine == Paper) then
    if (enemy == Rock) then
      Win
    else if (enemy == Scissors) then
      Lose 
    else
      Draw
  else if (mine == Scissors) then
    if (enemy == Rock) then
      Lose
    else if (enemy == Paper) then
      Win
    else
      Draw
  else
    Draw

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Rock Paper Scissors elm"
    , body =
      [ 
        div []
        [ viewResult model.result
        , div[] [text("Mine: " ++ viewHand model.mine)]
        , div[] [text("Enemy: " ++ viewHand model.enemy)]
        , button [ onClick (Next Rock) ] [ text "Rock" ]
        , button [ onClick (Next Paper) ] [ text "Paper" ]
        , button [ onClick (Next Scissors) ] [ text "Scissors" ]
        ]
      ]
    }

viewHand : Hand -> String
viewHand resultHand = 
  if (resultHand == Rock) then
    "rock"
  else if (resultHand == Paper) then
    "paper"
  else if (resultHand == Scissors) then
    "scissors"
  else 
    ""

viewResult : GameResult -> Html msg 
viewResult gameResult = 
  if (gameResult == Win) then
    div[ style "color" "green" ] [text("Win")]
  else if (gameResult == Lose) then
    div[ style "color" "red" ] [text("Lose")]
  else if (gameResult == Draw) then
    div[ style "color" "gray" ] [text("Draw")]
  else 
    div[] []