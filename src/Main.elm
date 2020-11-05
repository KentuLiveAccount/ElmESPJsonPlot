module Main exposing (..)

import Debug exposing (toString)
import Browser
import Html exposing (Html, button, text, div, h2)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, list, string, int, map3, decodeString, maybe)

-- MAIN
main = Browser.element { init = init, update = update, subscriptions = subscriptions, view = view}

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- MODEL
type Model = Loading | Failure String  | Success String

init : () -> (Model, Cmd Msg)
init _ = (Loading, getJson)

-- VIEW
view : Model -> Html Msg
view model = div [] [ h2 [] [ text "Random Cats" ], renderModel model]

renderModel : Model -> Html Msg
renderModel model = case model of
   Loading -> text "Loading ..."
   Failure str -> div [] [ text ("err: " ++ str), button [ onClick GetList ] [ text "Try Again!" ] ]
   Success str -> div [] [ text str]

-- UPDATE
type Msg = GetList | GotList (Result Http.Error (String))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
    GetList -> (Loading, getJson)
    GotList result ->  case result of
        Ok str -> (Success str, Cmd.none)
        Err err  -> (Failure (Debug.toString err), Cmd.none)

getJson : Cmd Msg
getJson = Http.get
    { url = "https://192.168.1.39/json"
    , expect = Http.expectJson GotList decodeList
    }

decodeList : Decoder (String)
decodeList = field "message" Json.Decode.string