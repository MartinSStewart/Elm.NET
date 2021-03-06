port module Main exposing (main)

import Browser
import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.Syntax.File as File
import Json.Decode as Decode
import Json.Encode as Encode


type alias Files =
    List String


type alias Response =
    Maybe (List File.File)


port response : Encode.Value -> Cmd msg


parseFiles : Files -> Response
parseFiles codeFiles =
    let
        rawFiles =
            codeFiles
                |> List.map Parser.parse
                |> List.filterMap Result.toMaybe
    in
    --if List.length rawFiles == List.length codeFiles then
    rawFiles
        |> List.foldl
            (\a b -> Processing.addFile a b)
            Processing.init
        |> (\a -> List.map (Processing.process a) rawFiles)
        |> Just



-- else
--     let
--         _ =
--             codeFiles
--                 |> List.map Parser.parse
--                 |> List.indexedMap (\index a -> Result.map (\b -> index) a)
--                 |> Debug.log ""
--     in
--     Nothing


encodeResponse : Response -> Encode.Value
encodeResponse a =
    case a of
        Just b ->
            Encode.list File.encode b

        Nothing ->
            Encode.null


decodeFlags : Decode.Decoder Files
decodeFlags =
    Decode.list Decode.string


debugLogMap : String -> (a -> String) -> a -> a
debugLogMap tag log value =
    let
        _ =
            value |> log |> Debug.log tag
    in
    value


main : Program Decode.Value () ()
main =
    Platform.worker
        { init =
            \fileText ->
                ( ()
                , fileText
                    |> Decode.decodeValue decodeFlags
                    |> Result.toMaybe
                    |> Maybe.andThen parseFiles
                    |> encodeResponse
                    |> response
                )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
