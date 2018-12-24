module Main exposing (main)

import Browser
import Html exposing (..)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Kbc Index Explorer", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    String


type Msg
    = NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( "Tomas", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text "Hello "
        , text model
        ]
