
module Main exposing (..)

-- import Html.Attributes exposing (..)
import Html exposing (Html, div)
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

type Msg = Redraw | XOClicked Int Int | Restart

type alias Model = {
  pospairs : List (Int, Int, Int),
  currentPlayer : Int
}

initialModel : Model
initialModel =
  { pospairs =
    [(0,0,0),(100,0,0),(200,0,0)
    ,(0,100,0),(100,100,0),(200,100,0)
    ,(0,200,0),(100,200,0),(200,200,0)
    ]
  , currentPlayer = 1
  }

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Redraw ->
      (model, Cmd.none)
    XOClicked int val ->
      if val == 0 then
        (model |> (toggleXOfield int) |> swapPlayer, Cmd.none)
      else
        (model, Cmd.none)
    Restart ->
      (initialModel, Cmd.none)

chooseColor : Int -> String
chooseColor val =
  case val of
    1 -> "red"
    2 -> "green"
    _ -> "black"

makeXOblock : Int -> (Int, Int, Int) -> Svg Msg
makeXOblock index other =
  let
   (xp,yp,val) = other
  in
    rect [x (toString xp), y (toString yp), width "100", height "100", fill (chooseColor val), onClick (XOClicked index val)] []

view : Model -> Html Msg
view model =
  div [] [
    Html.button [Html.Events.onClick Restart] [ Html.text "Restart" ],
    div [] [ Html.text (toString model.currentPlayer) ],
    svg [ width "300", height "300", viewBox "0 0 300 300" ]
      (List.indexedMap makeXOblock model.pospairs)
  ]
