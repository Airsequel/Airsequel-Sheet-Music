module Types.Song exposing
  ( Song
  , SongsPage
  , songDecoder
  , songsDecoder
  , songsPageDecoder
  )

import Html exposing (..)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Types.File exposing (File, fileDecoder)


type alias Song =
  { rowid : Int
  , name : String
  , instrumentation : Maybe String
  , style : Maybe String
  , tempo : Maybe String
  , key : Maybe String
  , interpreter : Maybe String
  , composer : Maybe String
  , arranger : Maybe String
  , description : Maybe String
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
    |> JDP.optional "style" (JD.maybe JD.string) Nothing
    |> JDP.required "tempo" (JD.maybe JD.string)
    |> JDP.required "key" (JD.maybe JD.string)
    |> JDP.required "interpreter" (JD.maybe JD.string)
    |> JDP.optional "composer" (JD.maybe JD.string) Nothing
    |> JDP.optional "arranger" (JD.maybe JD.string) Nothing
    |> JDP.optional "description" (JD.nullable JD.string) Nothing
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


{-| One page of the `songs_paginated_json` view.
Every row carries the window-function column `total_count`,
so the overall number of songs is read from the first row.
-}
type alias SongsPage =
  { songs : List Song
  , totalCount : Int
  }


songsPageDecoder : Decoder SongsPage
songsPageDecoder =
  JD.map2
    SongsPage
    (JD.list (songDecoder False))
    (JD.list (JD.field "total_count" JD.string)
      |> JD.map
          (List.head
            >> Maybe.andThen String.toInt
            >> Maybe.withDefault 0
          )
    )
