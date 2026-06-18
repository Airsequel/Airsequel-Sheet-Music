module Types.File exposing
  ( File
  , audioFiletypes
  , fileDecoder
  , imageFiletypes
  , isAudio
  , isImage
  , isPdf
  )

import Json.Decode as JD exposing (Decoder)


type alias File =
  { rowid : Int
  , name : Maybe String
  , filetype : Maybe String
  }


fileDecoder : Decoder File
fileDecoder =
  JD.map3
    File
    (JD.field "rowid" JD.int)
    (JD.maybe (JD.field "name" JD.string))
    (JD.field "filetype" (JD.maybe JD.string))


audioFiletypes : List String
audioFiletypes =
  [ "mp3", "m4a", "aac", "ogg", "oga", "opus", "wav", "flac" ]


isAudio : File -> Bool
isAudio file =
  case file.filetype of
    Just filetype ->
      List.member (String.toLower filetype) audioFiletypes
    Nothing ->
      False


imageFiletypes : List String
imageFiletypes =
  [ "png", "jpg", "jpeg", "gif", "webp", "bmp", "svg", "tif", "tiff", "avif" ]


isImage : File -> Bool
isImage file =
  case file.filetype of
    Just filetype ->
      List.member (String.toLower filetype) imageFiletypes
    Nothing ->
      False


isPdf : File -> Bool
isPdf file =
  (file.filetype |> Maybe.map String.toLower) == Just "pdf"
