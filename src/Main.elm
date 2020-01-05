import Browser
import Html exposing (Html, button, div, text, img)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random

-- MAIN

main =
    Browser.element
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

view : Model -> Html Msg
view model =
      div [ class "app" ]
      [ div[ class "enemy"] [ img[ src (viewHand model.enemy) ][]]
      , div[ class "result"] [ viewResult model.result ]
      , div[ class "mine"] [ img[ src (viewHand model.mine) ][]]
      , div[ class "buttons" ]
        [ div[ class "rock", onClick (Next Rock) ] [ img[ src "/img/rock.svg"][] ]
        , div[ class "paper", onClick (Next Paper) ] [ img[ src "/img/paper.svg"][] ]
        , div[ class "scissors", onClick (Next Scissors) ] [ img[ src "/img/scissors.svg"][] ]
        ]
      ]

viewHand : Hand -> String
viewHand resultHand = 
  if (resultHand == Rock) then
    "/img/rock.svg"
  else if (resultHand == Paper) then
    "/img/paper.svg"
  else if (resultHand == Scissors) then
    "/img/scissors.svg"
  else 
    ""

viewResult : GameResult -> Html msg 
viewResult gameResult = 
  if (gameResult == Win) then
    div[ class "win" ] [text("Win")]
  else if (gameResult == Lose) then
    div[ class "lose" ] [text("Lose")]
  else if (gameResult == Draw) then
    div[ class "draw" ] [text("Draw")]
  else 
    div[] []