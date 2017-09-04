
module Main exposing (..)

-- import Html.Attributes exposing (..)
import Html exposing (Html, div)
import Html.Events
import Html.Attributes as Ha
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

import Array
import Task

type Msg = XOClicked Int Int | Restart | CheckWinner

type alias Model = {
  pospairs : List (Int, Int, Int),
  currentPlayer : Int,
  first : Int,
  second : Int
}

initialModel : Model
initialModel =
  { pospairs =
    [(0,0,0),(100,0,0),(200,0,0)
    ,(0,100,0),(100,100,0),(200,100,0)
    ,(0,200,0),(100,200,0),(200,200,0)
    ]
  , currentPlayer = 1
  , first = 0
  , second = 0
  }

numToWinner : Int -> Winner
numToWinner int =
  case int of
    1 -> First
    2 -> Second
    _ -> Noone

extractAndCompare : Array.Array Int -> (Int, Int, Int) -> Winner
extractAndCompare array (t1, t2, t3) =
  let
    fst = Array.get t1 array
    snd = Array.get t2 array
    trd = Array.get t3 array
  in
    if (fst == snd) && (snd == trd) then
      case fst of
        Just x -> numToWinner x
        Nothing -> Noone -- shouldn't happen
    else
      Noone

type Winner = First | Second | Noone
whoWon : List (Int, Int, Int) -> Winner
whoWon lst =
  let
    array = Array.fromList (List.map (\(v1,v2,v3) -> v3) lst)
    checks = [
      (0,1,2),(3,4,5),(6,7,8), -- left-right
      (0,3,6),(1,4,7),(2,5,8), -- top-down
      (0,4,8),(2,4,6) -- diagonal
    ]
    flst = (List.map (extractAndCompare array) checks) -- final list
  in
    if List.member First flst then First else (if List.member Second flst then Second else Noone)


init : (Model, Cmd a)
init =
  (initialModel, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main =
  Html.program {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
  }

swapPlayer : Model -> Model
swapPlayer model =
  let
    newCP =
      case model.currentPlayer of
        1 -> 2
        _ -> 1
  in
    { model | currentPlayer = newCP }

updateFieldValue : Int -> Model -> Int -> (Int,Int,Int) -> (Int,Int,Int)
updateFieldValue index model wantedIndex other =
  let
    (xp,yp,val) = other
  in
    (if index == wantedIndex then (xp,yp,model.currentPlayer) else (xp,yp,val))

toggleXOfield : Int -> Model -> Model
toggleXOfield int model =
  { model | pospairs = (List.indexedMap (updateFieldValue int model) model.pospairs) }

addFirstPoint : Model -> Model
addFirstPoint model =
  { model | first = model.first + 1 }

addSecondPoint : Model -> Model
addSecondPoint model =
  { model | second = model.second + 1 }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    XOClicked int val ->
      if val == 0 then
        (model |> (toggleXOfield int) |> swapPlayer |> update CheckWinner)
      else
        (model, Cmd.none)
    CheckWinner ->
      case whoWon model.pospairs of
        First -> (model |> addFirstPoint |> update Restart)
        Second -> (model |> addSecondPoint |> update Restart)
        Noone -> (model, Cmd.none)
    Restart ->
      ({ initialModel | first = model.first, second = model.second }, Cmd.none)

chooseShape : Int -> (Int, Int, Int) -> Svg Msg
chooseShape index other =
  let
    (xp,yp,val) = other
  in
    case val of
      1 -> circle [cx (toString (xp+50)), cy (toString (yp+50)), r "30", fill "black", onClick (XOClicked index val)] []
      2 -> g [x (toString xp), y (toString yp), width "100", height "100", fill "black", onClick (XOClicked index val)] [
        line [x1 (toString (xp + 20)), y1 (toString (yp + 20)), x2 (toString (xp+80)), y2 (toString (yp+80)), stroke "black", strokeWidth "10"] [],
        line [x1 (toString (xp + 20)), y1 (toString (yp + 80)), x2 (toString (xp+80)), y2 (toString (yp+20)), stroke "black", strokeWidth "10"] []
      ]
      _ -> rect [x (toString xp), y (toString yp), width "100", height "100", fill "white", onClick (XOClicked index val)] []

makeXOblock : Int -> (Int, Int, Int) -> Svg Msg
makeXOblock index other =
  chooseShape index other

view : Model -> Html Msg
view model =
  div [Ha.align "center"] [
    Html.h1 [Ha.align "center"] [Html.text "Tic-Tac-Toe"],
    svg [ width "300", height "300", (viewBox "0 0 300 300"), (Ha.style [("border", "1px solid black")]) ]
      (List.indexedMap makeXOblock model.pospairs),
    div [] [ Html.button [Html.Events.onClick Restart] [ Html.text "Restart" ] ],
    div [Ha.style [("display","flex"), ("width", "20%")]] [
      Html.h2 [Ha.style [("flex","1")]] [Html.text ("◯ " ++ toString model.first)],
      Html.h2 [Ha.style [("flex","1")]] [Html.text ("🞫 " ++ toString model.second)]
    ]
  ]
