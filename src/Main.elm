module Main exposing (..)

import Debug exposing (toString)
import Browser
import Html exposing (Html, button, text, input, div, h2, br)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, list, string, int, map2, decodeString, maybe)
import LineChart exposing (viewAsLineChart)
import TypedSvg.Core exposing (Svg)

-- MAIN
main = Browser.element { init = init, update = update, subscriptions = subscriptions, view = view}

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- MODEL

type alias Temps =
    { current : Basics.Int
    , target : Basics.Int
    }
type Model = Loading | Failure String  | Success (List Temps)

init : () -> (Model, Cmd Msg)
init _ = (Loading, getJson)

-- VIEW
view : Model -> Html Msg
view model = div [] [ h2 [] [ text "Themometer" ], renderModel model]

genSeq : Int -> List Basics.Int
genSeq i = genSeq_ 1 i

genSeq_ : Int -> Int -> List Basics.Int
genSeq_ i mx =
    case i == mx of
       True -> [i]
       False ->  i :: (genSeq_ (i + 1) mx)

viewTempsPlot : List Temps -> Svg Msg
viewTempsPlot temps = 
    let
        currents = List.map (\x -> x.current) temps
        targets = List.map (\x -> x.target) temps
        xrange = genSeq (List.length currents)
    in
    viewAsLineChart 0 0 900 900 40 "BBQ Temp" "stragith from your BBQ"  [("Temperature", currents), ("Target", targets)] xrange (0, 1) 0 0

renderModel : Model -> Html Msg
renderModel model = case model of
   Loading -> text "Loading ..."
   Failure str -> div [] [ text ("err: " ++ str), button [ onClick GetList ] [ text "Refresh" ] ]
   Success temps -> div [] [button [ onClick GetList ] [ text "Refresh" ], input [type_ "text", value "250"] [], button [onClick (UpdateTemp 250)] [text "Set Temperature"], br [] [], viewTempsPlot temps, text (Debug.toString (List.map (\x -> x.current) temps))]

-- UPDATE
type Msg = GetList | GotList (Result Http.Error (List Temps)) | UpdateTemp Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
    GetList -> (Loading, getJson)
    GotList result ->  case result of
        Ok lst -> (Success lst, Cmd.none)
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
