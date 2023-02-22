module Types.Song exposing (..)

import GraphQL
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error(..))
import Json.Decode as JD exposing (Decoder)
import Types.File exposing (File)


fileDecoder : Decoder File
fileDecoder =
    JD.map2 File
        (JD.field "rowid" JD.int)
        (JD.field "filetype" JD.string)


type alias Song =
    { rowid : Int
    , name : String
    , instrumentation : Maybe String
    , tempo : Maybe String
    , key : Maybe String
    , interpreter : Maybe String
    , files : List File
    }


songDecoder : Bool -> Decoder Song
songDecoder withFiles =
    JD.map7 Song
        (JD.field "rowid" JD.int)
        (JD.field "name" JD.string)
        (JD.field "instrumentation" (JD.maybe JD.string))
        (JD.field "tempo" (JD.maybe JD.string))
        (JD.field "key" (JD.maybe JD.string))
        (JD.field "interpreter" (JD.maybe JD.string))
        (if withFiles then
            JD.field "files" JD.string
                |> JD.andThen
                    (\filesJson ->
                        case
                            filesJson
                                |> JD.decodeString (JD.list fileDecoder)
                        of
                            Ok files ->
                                JD.succeed files

                            Err jsonError ->
                                JD.errorToString jsonError
                                    |> JD.fail
                    )

         else
            JD.succeed []
        )


songsDecoder : Bool -> Decoder (List Song)
songsDecoder withFiles =
    JD.list (songDecoder withFiles)
