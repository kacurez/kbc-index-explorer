module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Http
import Json.Decode as Decode
    exposing
        ( Decoder
        , bool
        , dict
        , float
        , int
        , lazy
        , list
        , map
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
    | Success JsonValue
    | Failure String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , loadData
    )



-- UPDATE


type Msg
    = NoOp
    | GotData (Result Http.Error JsonValue)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData result ->
            case result of
                Ok fullText ->
                    ( Success fullText, Cmd.none )

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

        Success json ->
            viewJson json

        Failure _ ->
            div [] [ text "there was an error" ]


viewJson : JsonValue -> Html Msg
viewJson json =
    case json of
        JsonString value ->
            div [] [ text ("\"" ++ value ++ "\",\n") ]

        JsonInt value ->
            div [] [ text (String.fromInt value ++ ",") ]

        JsonFloat value ->
            div [] [ text (String.fromFloat value ++ ",") ]

        JsonBoolean value ->
            div [] [ text "boolean" ]

        JsonArray value ->
            div [] [ text "[", div [] (List.map viewJson value), text "]" ]

        JsonObject value ->
            div [] (List.map (\( k, v ) -> div [] [ text ("\"" ++ k ++ "\""), viewJson v ]) (Dict.toList value))

        JsonNull ->
            div [] [ text "null" ]



-- HTTP


loadData : Cmd Msg
loadData =
    Http.get
        { url = "https://connection.keboola.com/v2/storage"
        , expect = Http.expectJson GotData decoder
        }


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
