module Main exposing (JsonValue(..), Model(..), Msg(..), decoder, indent, init, loadData, main, update, view, viewJson, viewJsonKeyValuePair)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (style)
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


type alias KbcIndex =
    { host : String
    , api : String
    , version : String
    , revision : String
    , documentation : String
    , components : JsonValue
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


type Model
    = Loading
    | Success KbcIndex
    | Failure String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , loadData
    )



-- UPDATE


type Msg
    = NoOp
    | GotData (Result Http.Error KbcIndex)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData result ->
            case result of
                Ok kbcIndex ->
                    ( Success kbcIndex, Cmd.none )

                Err _ ->
                    ( Failure "there was a problem", Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            div [] [ text "Loading" ]

        Success kbcIndex ->
            viewJson 1 kbcIndex.components

        Failure _ ->
            div [] [ text "there was an error" ]


indent : Int -> Attribute msg
indent depth =
    style "padding-left" (String.fromInt depth ++ "em")


viewJson : Int -> JsonValue -> Html Msg
viewJson depth json =
    case json of
        JsonString value ->
            span [] [ text ("\"" ++ value ++ "\",\n") ]

        JsonInt value ->
            span [] [ text (String.fromInt value ++ ",") ]

        JsonFloat value ->
            span [] [ text (String.fromFloat value ++ ",") ]

        JsonBoolean value ->
            span [] [ text "boolean" ]

        JsonArray value ->
            span [] [ text "[", div [ indent depth ] (List.map (viewJson (depth + 1)) value), text "]" ]

        JsonObject value ->
            span [] (List.concat [ [ text "{" ], List.map (viewJsonKeyValuePair (depth + 1)) (Dict.toList value), [ text "}" ] ])

        JsonNull ->
            span [] [ text "null" ]


viewJsonKeyValuePair : Int -> ( String, JsonValue ) -> Html Msg
viewJsonKeyValuePair depth ( jsKey, jsValue ) =
    div [ indent depth ]
        [ text ("\"" ++ jsKey ++ "\": ")
        , viewJson depth jsValue
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


kbcIndexDecoder : Decoder KbcIndex
kbcIndexDecoder =
    map8 KbcIndex
        (field "host" string)
        (field "api" string)
        (field "version" string)
        (field "revision" string)
        (field "documentation" string)
        (field "components" decoder)
        (field "services" decoder)
        (field "urlTemplates" decoder)
