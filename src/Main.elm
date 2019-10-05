module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes exposing (src)
import Task



---- MODEL ----


type alias Model =
    Maybe String


init : ( Model, Cmd Msg )
init =
    ( Nothing, Cmd.none )



---- UPDATE ----


type Msg
    = Click
    | Clear
    | CsvLoad File
    | CsvRead String


requestCsv : Cmd Msg
requestCsv =
    Select.file [ "text/csv" ] CsvLoad


read : File -> Cmd Msg
read file =
    Task.perform CsvRead (File.toString file)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( model, requestCsv )

        Clear ->
            ( Nothing, Cmd.none )

        CsvLoad file ->
            ( model, read file )

        CsvRead str ->
            ( Just str, Cmd.none )



---- VIEW ----


showUpload : Model -> Element msg
showUpload model =
    let
        txt =
            model |> Maybe.withDefault "Nothing yet"
    in
    el [ centerX ] <| text txt


clearUpload : Model -> Element Msg
clearUpload model =
    case model of
        Nothing ->
            none

        Just _ ->
            Input.button [ padding 5, centerX, Background.color <| rgb 0.5 0.5 0.5 ] { onPress = Just Clear, label = text "Clear upload" }


view : Model -> Html Msg
view model =
    layout [ width fill, padding 20 ] <|
        column
            [ centerX
            , width <| px 300
            , spacing 10
            ]
            [ Input.button
                [ Background.image "/logo.svg"
                , padding 30
                , centerX
                ]
                { onPress = Just Click, label = none }
            , el [ Font.center, centerX ] <| text "Click the elm logo to upload a .csv"
            , clearUpload model
            , showUpload model
            ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
