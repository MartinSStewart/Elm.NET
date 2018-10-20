module Main exposing (main)

import Browser
import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.Syntax.File as File
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Encode as Encode


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    model


textFile =
    "module Test exposing (initialModel)\n\ntype alias Model =\n    {}\n\n\ninitialModel : Model\ninitialModel =\n    {}\n\n\ntype Msg\n    = NoOp\n\n\nupdate : Msg -> Model -> Model\nupdate msg model =\n    model"


view : Model -> Html Msg
view model =
    let
        result =
            Parser.parse textFile
    in
    case result of
        Ok rawFile ->
            Processing.process Processing.init rawFile
                |> File.encode
                |> Encode.encode 4
                |> text

        Err error ->
            text "Error"


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
