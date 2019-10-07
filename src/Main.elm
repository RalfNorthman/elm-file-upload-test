module Main exposing (main)

import Browser
import Csv
import Csv.Decode
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
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


type alias Record =
    { id : Int
    , name : String
    , parentId : Maybe Int
    }


record : Csv.Decode.Decoder Record
record =
    Csv.Decode.map3 Record
        Csv.Decode.int
        Csv.Decode.string
        (Csv.Decode.maybe Csv.Decode.int)


numberField : Maybe Int -> Element msg
numberField x =
    Maybe.map String.fromInt x
        |> Maybe.withDefault ""
        |> text
        |> el [ Font.alignRight ]


makeTable : List Record -> Element msg
makeTable records =
    el [ centerX ] <|
        table
            [ spacing 10
            , Font.alignLeft
            ]
            { data = records
            , columns =
                [ { header = text "Id"
                  , width = shrink
                  , view = \r -> numberField <| Just r.id
                  }
                , { header = text "Name"
                  , width = shrink
                  , view = \r -> text r.name
                  }
                , { header = text "Parent Id"
                  , width = shrink
                  , view = \r -> numberField r.parentId
                  }
                ]
            }


dataTable : Model -> Element msg
dataTable model =
    case model of
        FileStr str ->
            let
                csv =
                    Csv.parse str

                decoded =
                    Csv.Decode.decode record csv
            in
            case decoded of
                Ok records ->
                    makeTable records

                Err error ->
                    column [ spacing 10 ]
                        [ text "Something went wrong:"
                        , text <| Debug.toString error
                        ]

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
