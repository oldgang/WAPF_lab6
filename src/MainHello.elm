module MainHello exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Time exposing (..)
import Json.Decode exposing (succeed, fail, andThen)
import Http
import Json.Decode exposing (..)

-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL

type alias DateTime = { zone : Time.Zone, time : Time.Posix }

type alias Model = { 
    dateTime : DateTime,
    searchText : String
    } 



init : () -> ( Model, Cmd Msg )
init _ = ( 
    Model (DateTime Time.utc (Time.millisToPosix 0)) "" 
    ,Cmd.batch [ Task.perform AdjustTimeZone Time.here, Task.perform Tick Time.now ]
    )



-- UPDATE


type Msg  = 
    Tick Time.Posix 
    | AdjustTimeZone Time.Zone
    | UpdateField String
    | Search


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime -> 
            ( { model | dateTime = DateTime model.dateTime.zone newTime }, Cmd.none )
        AdjustTimeZone newZone ->
            ( { model | dateTime = DateTime newZone model.dateTime.time }, Cmd.none )
        UpdateField searchText ->
            ( { model | searchText = searchText }, Cmd.none)
        Search -> ( model, Nav.load ("https://google.com/search?q="++ model.searchText))


weatherApi = "https://api.openweathermap.org/data/2.5/weather?"
apiKey = "483772c027b65e249d5984f627a89ad5"


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ = 
    Time.every 1000 Tick



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Hello"
    , body = [ viewTime model.dateTime
             , viewDate model.dateTime
             , viewSearchBar]
    }


onEnter msg =
    let
        isEnter code = 
            if code == 13 then 
            succeed msg
            else 
            fail "not ENTER"
    in
    on "keydown" (andThen isEnter keyCode)

viewSearchBar = div []
        [ 
        input [ type_ "text", 
                placeholder "Search", 
                onInput UpdateField, 
                onEnter Search]
                []
        ]

viewTime dateTime = 
    let
        hour = Time.toHour dateTime.zone dateTime.time
        minute = Time.toMinute dateTime.zone dateTime.time
        second = Time.toSecond dateTime.zone dateTime.time
    in
    div [] [text (formatNumbers hour ++ ":" ++ formatNumbers minute ++ ":" ++ formatNumbers second)]

viewDate dateTime =
    let 
        weekday = Time.toWeekday dateTime.zone dateTime.time
        day = Time.toDay dateTime.zone dateTime.time
        month = Time.toMonth dateTime.zone dateTime.time
        year  = Time.toYear dateTime.zone dateTime.time
    in
    div [] [ text (toEnglishWeekday weekday 
                    ++ ", "
                    ++ String.fromInt day
                    ++ " "
                    ++ toEnglishMonth month
                    ++ " "
                    ++ String.fromInt year
                    ) ]

formatNumbers : Int -> String
formatNumbers number = 
    case String.length (String.fromInt number) of
        1 -> "0" ++ String.fromInt number
        _ -> String.fromInt number
    
toEnglishMonth : Month ->  String
toEnglishMonth month = 
    case month of
        Jan -> "January"
        Feb -> "February"
        Mar -> "March"
        Apr -> "April"
        May -> "May"
        Jun -> "June"
        Jul -> "July"
        Aug -> "August"
        Sep -> "September"
        Oct -> "October"
        Nov -> "November"
        Dec -> "December"

toEnglishWeekday : Weekday -> String
toEnglishWeekday weekday =
    case weekday of
        Mon -> "Monday"
        Tue -> "Tuesday"
        Wed -> "Wednesday"
        Thu -> "Thursday"
        Fri -> "Friday"
        Sat -> "Saturday"
        Sun -> "Sunday"