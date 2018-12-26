module Main exposing (JsonValue(..), Model, Msg(..), decoder, indent, init, loadData, main, update, view, viewJsonKeyValuePair, viewJsonValue)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy
import Http
import Json.Decode as Decode
    exposing
        ( Decoder
        , bool
        , dict
        , field
        , float
        , int
        , lazy
        , list
        , map
        , map2
        , map8
        , null
        , oneOf
        , string
        )



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Kbc Index Explorer", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { filter : String
    , apiData : ApiData
    }


type alias Component =
    { id : String
    , json : JsonValue
    }


type alias KbcIndex =
    { host : String
    , api : String
    , version : String
    , revision : String
    , documentation : String
    , components : List Component
    , services : JsonValue
    , urlTemplates : JsonValue
    }


type JsonValue
    = JsonString String
    | JsonInt Int
    | JsonFloat Float
    | JsonBoolean Bool
    | JsonArray (List JsonValue)
    | JsonObject (Dict String JsonValue)
    | JsonNull


type ApiData
    = Loading
    | Success KbcIndex
    | Failure String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Loading
    , loadData
    )



-- UPDATE


type Msg
    = NoOp
    | ChangeFilter String
    | Refresh
    | GotData (Result Http.Error KbcIndex)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData result ->
            case result of
                Ok kbcIndex ->
                    ( { model | apiData = Success kbcIndex }, Cmd.none )

                Err _ ->
                    ( { model | apiData = Failure "there was a problem" }, Cmd.none )

        ChangeFilter filter ->
            ( { model | filter = filter }, Cmd.none )

        Refresh ->
            ( { model | apiData = Loading }, loadData )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewFilter model
        , viewRefreshButton
        , case model.apiData of
            Loading ->
                div [] [ text "Loading" ]

            Success kbcIndex ->
                viewComponents (filterComponents model.filter kbcIndex.components)

            Failure _ ->
                div [] [ text "there was an error" ]
        ]


viewRefreshButton : Html Msg
viewRefreshButton =
    button [ onClick Refresh ] [ text "Reload" ]


viewFilter : Model -> Html Msg
viewFilter model =
    input [ placeholder "filter values", value model.filter, onInput ChangeFilter ] []


indent : Int -> Attribute msg
indent depth =
    style "padding-left" ((++) (String.fromFloat (0.5 * toFloat depth)) "em")


viewComponents : List Component -> Html Msg
viewComponents components =
    div []
        [ text ("count:" ++ String.fromInt (List.length components))
        , Keyed.node "div" [] (List.map viewComponentKeyed components)
        ]


viewComponentKeyed : Component -> ( String, Html Msg )
viewComponentKeyed component =
    ( component.id, Html.Lazy.lazy2 viewJsonValue 1 component.json )


viewJsonValue : Int -> JsonValue -> Html Msg
viewJsonValue depth json =
    case json of
        JsonString value ->
            viewStringValue ("\"" ++ value ++ "\"")

        JsonInt value ->
            viewStringValue (String.fromInt value)

        JsonFloat value ->
            viewStringValue (String.fromFloat value)

        JsonBoolean value ->
            viewStringValue (boolToString value)

        JsonNull ->
            viewStringValue "null"

        JsonArray value ->
            span []
                [ text "["
                , div [ indent depth ] (List.map (viewJsonValue (depth + 1)) value)
                , text "],"
                ]

        JsonObject value ->
            span []
                [ text "{"
                , span []
                    (value
                        |> Dict.toList
                        |> List.map (viewJsonKeyValuePair (depth + 1))
                    )
                , text "},"
                ]


viewStringValue : String -> Html Msg
viewStringValue value =
    span [] [ text (value ++ ",") ]



{--
    if shouldNotFilter filter then
        span [] [ text (value ++ ",") ]

    else
        let
            values =
                String.split filter value
        in
        span []
            ((values
                |> List.map text
                |> List.intersperse (span [ style "background-color" "yellow" ] [ text filter ])
                |> List.append
             )
             <|
                [ text "," ]
            )
--}


viewJsonKeyValuePair : Int -> ( String, JsonValue ) -> Html Msg
viewJsonKeyValuePair depth ( jsKey, jsValue ) =
    let
        component =
            if jsKey == "id" then
                strong

            else
                div
    in
    component [ indent depth ]
        [ text ("\"" ++ jsKey ++ "\": ")
        , viewJsonValue depth jsValue
        ]



-- HTTP


loadData : Cmd Msg
loadData =
    Http.get
        { url = "https://connection.keboola.com/v2/storage"
        , expect = Http.expectJson GotData kbcIndexDecoder
        }



-- https://stackoverflow.com/questions/40825493/elm-decoding-unknown-json-structure


decoder : Decoder JsonValue
decoder =
    oneOf
        [ map JsonString string
        , map JsonInt int
        , map JsonFloat float
        , map JsonBoolean bool
        , list (lazy (\_ -> decoder)) |> map JsonArray
        , dict (lazy (\_ -> decoder)) |> map JsonObject
        , null JsonNull
        ]


componentDecoder : Decoder Component
componentDecoder =
    map2 Component
        (field "id" string)
        decoder


kbcIndexDecoder : Decoder KbcIndex
kbcIndexDecoder =
    map8 KbcIndex
        (field "host" string)
        (field "api" string)
        (field "version" string)
        (field "revision" string)
        (field "documentation" string)
        (field "components" (list componentDecoder))
        (field "services" decoder)
        (field "urlTemplates" decoder)



-- HELPERS


boolToString : Bool -> String
boolToString value =
    if value == True then
        "true"

    else
        "false"


filterJsonValue : String -> JsonValue -> Bool
filterJsonValue filter jsonValue =
    case jsonValue of
        JsonString value ->
            String.contains filter value

        JsonInt value ->
            String.contains filter (String.fromInt value)

        JsonFloat value ->
            String.contains filter (String.fromFloat value)

        JsonBoolean value ->
            String.contains filter (boolToString value)

        JsonArray value ->
            List.any (filterJsonValue filter) value

        JsonObject value ->
            List.any (\( k, v ) -> filterJsonValue filter v) (Dict.toList value)

        JsonNull ->
            String.contains filter "null"


shouldNotFilter : String -> Bool
shouldNotFilter filter =
    String.isEmpty filter


filterComponents : String -> List Component -> List Component
filterComponents filter components =
    if shouldNotFilter filter then
        components

    else
        List.filter (\component -> String.contains filter component.id) components
