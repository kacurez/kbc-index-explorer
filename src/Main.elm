module Main exposing (JsonValue(..), Model, Msg(..), decoder, indent, init, loadData, main, update, view, viewJsonKeyValuePair, viewJsonValue)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
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
        , at
        , bool
        , dict
        , field
        , float
        , int
        , lazy
        , list
        , map
        , map3
        , map8
        , maybe
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
    { region : Region
    , filter : String
    , showSimple : Bool
    , data : Dict Region ApiData
    }


type alias Component =
    { id : String
    , dataDefinition : Maybe JsonValue
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


type alias Region =
    String


regionUrlsDict =
    Dict.fromList
        [ ( "EU", "https://connection.eu-central-1.keboola.com/v2/storage" )
        , ( "US", "https://connection.keboola.com/v2/storage" )
        , ( "Azure Multitenant", "https://connection.keboola.com/v2/storage" )
        , ( "Azure Testing", "https://connection.east-us-2.azure.keboola-testing.com/v2/storage" )
        , ( "csas", "https://connection.csas.keboola.com/v2/storage" )
        ]


type ApiData
    = Loading
    | Success KbcIndex
    | Failure String


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Model "US" "" True (Dict.fromList [ ( "US", Loading ) ])
    in
    ( model
    , loadData model.region
    )



-- UPDATE


type Msg
    = NoOp
    | ChangeFilter String
    | Refresh
    | ChangeRegion Region
    | ChangeView Bool
    | GotData Region (Result Http.Error KbcIndex)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData region result ->
            case result of
                Ok newData ->
                    ( { model | data = Dict.insert region (Success newData) model.data }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | data = Dict.insert region (Failure "there was a problem") model.data }
                    , Cmd.none
                    )

        ChangeFilter filter ->
            ( { model | filter = filter }, Cmd.none )

        Refresh ->
            ( { model
                | data =
                    model.data
                        |> Dict.insert "US" Loading
                        |> Dict.insert "EU" Loading
                        |> Dict.insert "Azure Testing" Loading
                        |> Dict.insert "Azure Multitenant" Loading
                        |> Dict.insert "csas" Loading
              }
            , Cmd.batch [ loadData "US", loadData "EU", loadData "Azure Testing", loadData "Azure Multitenant", loadData "csas" ]
            )

        ChangeView newValue ->
            ( { model | showSimple = not model.showSimple }, Cmd.none )

        ChangeRegion newRegion ->
            ( { model | region = newRegion }, loadRegion newRegion model )

        NoOp ->
            ( model, Cmd.none )


loadRegion : Region -> Model -> Cmd Msg
loadRegion region model =
    if Dict.member region model.data then
        Cmd.none

    else
        loadData region



-- VIEW


view : Model -> Html Msg
view model =
    Grid.container []
        -- Responsive fixed width container
        [ CDN.stylesheet
        , h2 [] [ text "Keboola Connection Components Index" ]
        , viewMainContent
            model

        -- Interactive and responsive menu
        ]


viewForm : Model -> Html Msg
viewForm model =
    Form.form []
        [ Form.row []
            [ Form.colLabel [ Col.sm2 ] [ text "Component id" ]
            , Form.col [ Col.sm6 ]
                [ Input.text
                    [ Input.attrs
                        [ placeholder "filter by component id"
                        , autofocus True
                        , value model.filter
                        , onInput ChangeFilter
                        ]
                    ]
                ]
            , Form.col [ Col.sm2 ]
                [ Button.button
                    [ Button.primary
                    , Button.attrs
                        [ type_ "button"
                        , onClick Refresh
                        ]
                    ]
                    [ text "Reload" ]
                ]
            ]
        , Form.row []
            [ Form.col [ Col.sm8 ]
                (Radio.radioList
                    "viewradios"
                    [ Radio.create
                        [ Radio.id "simple"
                        , Radio.inline
                        , Radio.checked model.showSimple
                        , Radio.onClick (ChangeView True)
                        ]
                        "Show data definitions only"
                    , Radio.create
                        [ Radio.id "whole"
                        , Radio.inline
                        , Radio.checked (not model.showSimple)
                        , Radio.onClick (ChangeView False)
                        ]
                        "Show whole json"
                    ]
                )
            ]
        , Form.row []
            [ Form.col [ Col.sm10 ]
                (Radio.radioList
                    "regionradio"
                    [ Radio.create
                        [ Radio.id "us"
                        , Radio.inline
                        , Radio.checked (model.region == "US")
                        , Radio.onClick (ChangeRegion "US")
                        ]
                        "US region"
                    , Radio.create
                        [ Radio.id "eu"
                        , Radio.inline
                        , Radio.checked (model.region == "EU")
                        , Radio.onClick (ChangeRegion "EU")
                        ]
                        "EU region"
                    , Radio.create
                        [ Radio.id "azure multitenant"
                        , Radio.inline
                        , Radio.checked (model.region == "Azure Multitenant")
                        , Radio.onClick (ChangeRegion "Azure Multitenant")
                        ]
                        "Azure Multitenant"
                    , Radio.create
                        [ Radio.id "azure testing"
                        , Radio.inline
                        , Radio.checked (model.region == "Azure Testing")
                        , Radio.onClick (ChangeRegion "Azure Testing")
                        ]
                        "Azure Testing"
                    , Radio.create
                        [ Radio.id "csas"
                        , Radio.inline
                        , Radio.checked (model.region == "csas")
                        , Radio.onClick (ChangeRegion "csas")
                        ]
                        "csas"
                    ]
                )
            ]
        ]


viewMainContent : Model -> Html Msg
viewMainContent model =
    div []
        [ viewForm model
        , case Maybe.withDefault Loading (Dict.get model.region model.data) of
            Loading ->
                div [] [ text "Loading" ]

            Success data ->
                Grid.row [] [ Grid.col [] [ viewComponents model (filterComponents model.filter data.components) ] ]

            Failure _ ->
                div [] [ text "there was an error" ]
        ]


indent : Int -> Attribute msg
indent depth =
    style "padding-left" ((++) (String.fromFloat (0.5 * toFloat depth)) "em")


viewComponents : Model -> List Component -> Html Msg
viewComponents model components =
    div []
        [ viewCount (List.length components)
        , if model.showSimple then
            viewComponentsSimplified components

          else
            Keyed.node "div" [] (List.map viewComponentKeyed components)
        ]


viewCount : Int -> Html Msg
viewCount count =
    div []
        [ text (String.fromInt count) ]


viewComponentsSimplified : List Component -> Html Msg
viewComponentsSimplified components =
    div []
        (components
            |> List.map (\c -> ( c.id, Maybe.withDefault JsonNull c.dataDefinition ))
            |> List.map (\( cid, dataDefinition ) -> div [] [ strong [] [ text cid ], text ": ", viewJsonValue 1 dataDefinition ])
        )


viewComponentKeyed : Component -> ( String, Html Msg )
viewComponentKeyed component =
    ( component.id, Html.Lazy.lazy2 viewJsonValue 1 component.json )


viewJsonValue : Int -> JsonValue -> Html Msg
viewJsonValue depth json =
    case json of
        JsonString value ->
            viewStringValue "green" ("\"" ++ value ++ "\"")

        JsonInt value ->
            viewStringValue "blue" (String.fromInt value)

        JsonFloat value ->
            viewStringValue "blue" (String.fromFloat value)

        JsonBoolean value ->
            viewStringValue "blue" (boolToString value)

        JsonNull ->
            viewStringValue "blue" "null"

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


viewStringValue : String -> String -> Html Msg
viewStringValue color value =
    span [ style "color" color ] [ text (value ++ ",") ]



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
        [ span [] [ text ("\"" ++ jsKey ++ "\": ") ]
        , viewJsonValue depth jsValue
        ]



-- HTTP


loadData : Region -> Cmd Msg
loadData region =
    Http.get
        { url = Maybe.withDefault "invalidurl" (Dict.get region regionUrlsDict)
        , expect = Http.expectJson (GotData region) kbcIndexDecoder
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
    map3 Component
        (field "id" string)
        (maybe (at [ "data", "definition" ] decoder))
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
