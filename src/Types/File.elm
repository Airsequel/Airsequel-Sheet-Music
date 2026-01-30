module Types.File exposing (File, fileDecoder)

import Json.Decode as JD exposing (Decoder)


type alias File =
  { rowid : Int
  , filetype : Maybe String
  }


fileDecoder : Decoder File
fileDecoder =
  JD.map2
    File
    (JD.field "rowid" JD.int)
    (JD.field "filetype" (JD.maybe JD.string))
