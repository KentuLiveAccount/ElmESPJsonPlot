module Main exposing (..)

import Debug exposing (toString)
import Browser
import Html exposing (Html, button, text, input, div, h2, h1, br)
import Html.Attributes exposing (type_, value, attribute, style)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, list, string, int, map3, decodeString, maybe)
import LineChart exposing (viewAsLineChart)
import TypedSvg.Core exposing (Svg)
import Time exposing (..)
import Task 

-- MAIN
main = Browser.element { init = init, update = update, subscriptions = subscriptions, view = view}

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- MODEL

type alias Temps =
    { current : Basics.Int
    , internal : Basics.Int
    , target : Basics.Int
    }

type alias TempState =
    { temps: List Temps
    , curTemp  :  Basics.Int
    , curIntern : Basics.Int
    , curTarget: Basics.Int
    , logs : List String
    }
type Model = Loading (List String)| Failure String  | Success TempState

init : () -> (Model, Cmd Msg)
init _ = (Loading [], getJson)

lgTxtStyle = [style "font-size" "2em", style "font-family" "Arial, Helvetica, sans-serif"]
lgCtrlStyle = [style "font-size" "1.1em", style "font-family" "Arial, Helvetica, sans-serif"]

smlogStyle = [style "font-size" "0.8em", style "font-family" "Arial, Helvetica, sans-serif"]
-- VIEW
view : Model -> Html Msg
view model = div lgTxtStyle [renderModel model]

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

        internals = List.map (\x -> x.internal) temps
        targets = List.map (\x -> x.target) temps
        xrange = genSeq (List.length currents)
    in
    viewAsLineChart 0 0 750 900 40 "" ""  [("Temp", currents), ("Internal", internals), ("Target", targets)] xrange (0, 1) 0 0

renderLogs : List String -> Html Msg
renderLogs logs = div ([style "display" "block", style "margin" "auto"] ++ smlogStyle) (List.intersperse (br [] []) <| (List.map (\x -> text x) logs))

renderModel : Model -> Html Msg
renderModel model = case model of
    Loading _ -> text "Loading ..."
    Failure str -> div [] [ text ("err: " ++ str), button [ onClick GetList ] [ text "Refresh" ] ]
    Success ts -> div 
        [style "display" "block", style "margin" "auto"] 
        [ text ("Current: " ++  String.fromInt ts.curTemp)
        , br [][]
        ,    div [style "display" "inline"] 
                [ text "Target: "
                , input ([type_ "text", value (String.fromInt ts.curTarget), onInput NewTarget] ++ lgCtrlStyle) []
                , button ([onClick (UpdateTemp ts.curTarget)] ++ lgCtrlStyle) [text "Refresh"]
                ]
        , br [][]
        , viewTempsPlot ts.temps
        , renderLogs ts.logs
        ] 

-- UPDATE
type Msg = GetList | GotList (Result Http.Error (List Temps)) | UpdateTemp Int | NewTarget String | UpdateTempWithTime Int String

tempStateFromList : List Temps -> List String -> TempState
tempStateFromList temps logs =
    let
        lastItem = Maybe.withDefault (Temps 0 0 0) <| List.head <| List.reverse temps
    in
    TempState temps lastItem.current lastItem.internal lastItem.target logs

updateTargetInModel : Model -> String -> Model
updateTargetInModel model str = case model of
   Loading _  -> model
   Failure _ -> model
   Success ts -> Success <| {ts | curTarget = (Maybe.withDefault (ts.curTarget) <| String.toInt str)}

logsFromModel : Model -> List String
logsFromModel model = case model of
   Loading logs  -> logs
   Failure _ -> []
   Success ts -> ts.logs

curTempFromModel : Model -> Int
curTempFromModel model = case model of
   Loading _ -> 0
   Failure _ -> 0
   Success ts -> ts.curTemp

intTempFromModel : Model -> Int
intTempFromModel model = case model of
   Loading _ -> 0
   Failure _ -> 0
   Success ts -> ts.curIntern
    
monthIndex: Month -> Int
monthIndex mo = case mo of
    Jan -> 1
    Feb -> 2
    Mar -> 3
    Apr -> 4
    May -> 5
    Jun -> 6
    Jul -> 7
    Aug -> 8
    Sep -> 9
    Oct -> 10
    Nov -> 11
    Dec -> 12

twoDigits : String -> String
twoDigits str = String.padLeft 2 '0' str

timeToString : Zone -> Posix -> String
timeToString zn pzx = 
    let
        mo = twoDigits <| String.fromInt <| monthIndex <| Time.toMonth zn pzx
        dy = twoDigits <| String.fromInt <|Time.toDay zn pzx
        hr = twoDigits <| String.fromInt <|Time.toHour zn pzx
        min = twoDigits <| String.fromInt <|Time.toMinute zn pzx
    in
        mo ++ "/" ++ dy ++ " " ++ hr ++ ":" ++ min

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
    GetList -> (Loading <| logsFromModel model, getJson)
    GotList result ->  case result of
        Ok lst -> (Success <| tempStateFromList lst (logsFromModel model), Cmd.none)
        Err err  -> (Failure (Debug.toString err), Cmd.none)
    UpdateTemp temp -> (model, Task.perform (UpdateTempWithTime temp) (Task.map2 timeToString Time.here Time.now))
    UpdateTempWithTime temp tm -> (Loading <| 
        ((tm ++ " Target: " ++ String.fromInt(temp) ++ ", Cur: " ++ String.fromInt(curTempFromModel model)) ++ ", Int: " ++ String.fromInt(intTempFromModel model))  :: logsFromModel model
        , updateTemp temp)
    NewTarget  str  -> (updateTargetInModel model str, Cmd.none)

getJson : Cmd Msg
getJson = Http.get
    { url = "http://192.168.1.4/json"
    , expect = Http.expectJson GotList decodeList
    }

decodeList : Decoder (List Temps)
decodeList = field "message" (Json.Decode.list 
    (map3 Temps
        (field "currenttemp" int)
        (field "internatemp" int)
        (field "targettemp" int)))

updateTemp : Int -> Cmd Msg
updateTemp temp = Http.post
    { url = "http://192.168.1.4/settemp"
    , body = Http.stringBody "text/plain" (String.fromInt temp)
    , expect = Http.expectWhatever (\r -> GetList)
    }
