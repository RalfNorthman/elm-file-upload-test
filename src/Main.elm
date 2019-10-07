module Main exposing (main)

import Browser
import Csv
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Task



---- MODEL ----


type FileError
    = TooBig
    | NotCsv


type Model
    = Idle
    | UserPicking
    | ReadingFile
    | FileError FileError
    | FileStr String


init : ( Model, Cmd Msg )
init =
    ( Idle, Cmd.none )



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


handle : File -> ( Model, Cmd Msg )
handle file =
    let
        bigFile =
            File.size file > 400000

        isCsv =
            File.mime file == "text/csv"
    in
    case ( bigFile, isCsv ) of
        ( True, _ ) ->
            ( FileError TooBig, Cmd.none )

        ( _, False ) ->
            ( FileError NotCsv, Cmd.none )

        _ ->
            ( ReadingFile, read file )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Click ->
            ( UserPicking, requestCsv )

        Clear ->
            ( Idle, Cmd.none )

        CsvLoad file ->
            handle file

        CsvRead str ->
            ( FileStr str, Cmd.none )



---- VIEW ----


uploadButton : Element Msg
uploadButton =
    column
        [ spacing 10
        , padding 10
        , centerX
        ]
        [ Input.button
            [ Background.image "/logo.svg"
            , padding 30
            , centerX
            ]
            { onPress = Just Click
            , label = none
            }
        , el
            []
          <|
            text "Click the elm logo to upload a .csv"
        ]


clearButton : Model -> Element Msg
clearButton model =
    case model of
        FileStr _ ->
            Input.button
                [ padding 5
                , centerX
                , Background.color <| rgb 0.2 0.5 0.7
                ]
                { onPress = Just Clear
                , label = text "Clear upload"
                }

        _ ->
            none


statusText : Model -> Element msg
statusText model =
    let
        str =
            case model of
                Idle ->
                    ""

                UserPicking ->
                    "A picking window should've popped up"

                ReadingFile ->
                    "File is being read"

                FileStr _ ->
                    "File loaded:"

                FileError error ->
                    case error of
                        TooBig ->
                            "File is too big"

                        NotCsv ->
                            "File is not a .csv"
    in
    el [ centerX ] <| text str


dataTable : Model -> Element msg
dataTable model =
    let
        makeTable : String -> Element msg
        makeTable str =
            let
                csv =
                    Csv.parse str
            in
            column []
                [ row [ spacing 10 ] <|
                    List.map text csv.headers
                , column [ spacing 10 ]
                    (List.map
                        (\lst ->
                            (\s -> row [ spacing 10 ] (List.map text s)) lst
                        )
                        csv.records
                    )
                ]
    in
    case model of
        FileStr str ->
            makeTable str

        _ ->
            none


view : Model -> Html Msg
view model =
    layout [ width fill, padding 20 ] <|
        column
            [ centerX
            , width <| px 300
            , spacing 10
            ]
            [ uploadButton
            , clearButton model
            , statusText model
            , dataTable model
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
