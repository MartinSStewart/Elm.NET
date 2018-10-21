module Main exposing (main)

import Browser
import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.Syntax.File as File
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onClick)
import Json.Encode as Encode


type alias Model =
    { fileText : Maybe String }


initialModel : Model
initialModel =
    { fileText = Nothing }


type Msg
    = TextChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextChanged newText ->
            { model | fileText = Just newText }


view : Model -> Html Msg
view model =
    let
        output =
            case model.fileText of
                Just text ->
                    case Parser.parse text of
                        Ok rawFile ->
                            Processing.process Processing.init rawFile
                                |> File.encode
                                |> Encode.encode 4

                        Err error ->
                            "Error"

                Nothing ->
                    ""
    in
    div
        []
        [ div []
            [ Html.textarea
                [ style "width" "600px"
                , style "height" "500px"
                , Html.Events.onInput TextChanged
                ]
                []
            ]
        , text output
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
