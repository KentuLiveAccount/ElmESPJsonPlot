module Main exposing (..)

import Debug exposing (toString)
import Browser
import Html exposing (Html, button, text, input, div, h2)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, list, string, int, map2, decodeString, maybe)

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
view model = div [] [ h2 [] [ text "Themometer" ], renderModel model]

type alias Temps =
    { current : Basics.Int
    , target : Basics.Int
    }

renderModel : Model -> Html Msg
renderModel model = case model of
   Loading -> text "Loading ..."
   Failure str -> div [] [ text ("err: " ++ str), button [ onClick GetList ] [ text "Refresh" ] ]
   Success str -> div [] [button [ onClick GetList ] [ text "Refresh" ], input [type_ "text", value "250"] [], button [onClick (UpdateTemp 250)] [text "Set Temperature"], text str]

-- UPDATE
type Msg = GetList | GotList (Result Http.Error (List Temps)) | UpdateTemp Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
    GetList -> (Loading, getJson)
    GotList result ->  case result of
        Ok lst -> (Success (Debug.toString lst), Cmd.none)
        Err err  -> (Failure (Debug.toString err), Cmd.none)
    UpdateTemp temp -> (Loading, updateTemp temp)

getJson : Cmd Msg
getJson = Http.get
    { url = "http://192.168.1.39/json"
    , expect = Http.expectJson GotList decodeList
    }

decodeList : Decoder (List Temps)
decodeList = field "message" (Json.Decode.list 
    (map2 Temps
        (field "currenttemp" int)
        (field "targettemp" int)))

updateTemp : Int -> Cmd Msg
updateTemp temp = Http.post
    { url = "http://192.168.1.39/settemp"
    , body = Http.stringBody "text/plain" (String.fromInt temp)
    , expect = Http.expectWhatever (\r -> GetList)
    }
