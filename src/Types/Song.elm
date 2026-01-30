module Types.Song exposing (Song, songDecoder, songsDecoder)

import Html exposing (..)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Types.File exposing (File, fileDecoder)


type alias Song =
  { rowid : Int
  , name : String
  , instrumentation : Maybe String
  , tempo : Maybe String
  , key : Maybe String
  , interpreter : Maybe String
  , numberOfFiles : Int
  , filetypes : Maybe String
  , files : List File
  , isFavorite : Bool
  }


filesDecoder : Bool -> Decoder (List File)
filesDecoder withFiles =
  if withFiles
    then JD.field "files" JD.string
      |> JD.andThen
          (\filesJson -> case filesJson
              |> JD.decodeString (JD.list fileDecoder) of
              Ok files ->
                JD.succeed files
              Err jsonError ->
                JD.errorToString jsonError
                  |> JD.fail
          )
    else JD.succeed []


songDecoder : Bool -> Decoder Song
songDecoder withFiles =
  JD.succeed Song
    |> JDP.required "rowid" JD.int
    |> JDP.required "name" JD.string
    |> JDP.required "instrumentation" (JD.maybe JD.string)
    |> JDP.required "tempo" (JD.maybe JD.string)
    |> JDP.required "key" (JD.maybe JD.string)
    |> JDP.required "interpreter" (JD.maybe JD.string)
    |> JDP.custom
        (JD.field "numberOfFiles" JD.string
          |> JD.map
              (\s -> String.toInt s
                  |> Maybe.withDefault 0
              )
        )
    |> JDP.required "filetypes" (JD.maybe JD.string)
    |> JDP.custom (filesDecoder withFiles)
    |> JDP.required "is_favorite" JD.bool


songsDecoder : Bool -> Decoder (List Song)
songsDecoder withFiles =
  JD.list (songDecoder withFiles)
