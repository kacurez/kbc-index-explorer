module Main exposing (main)

import Browser
import Html exposing (..)
import Http



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


type alias Data =
    String


type Model
    = Loading
    | Success Data
    | Failure String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get { url = "https://connection.keboola.com/v2/storage", expect = Http.expectString GotData }
    )



-- UPDATE


type Msg
    = NoOp
    | GotData (Result Http.Error String)


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

        Success fullText ->
            div [] [ text fullText ]

        Failure _ ->
            div [] [ text "there was an error" ]
