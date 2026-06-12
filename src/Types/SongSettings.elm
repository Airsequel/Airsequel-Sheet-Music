module Types.SongSettings exposing
  ( ColorScheme(..)
  , SongSettings
  , dictDecoder
  , encodeDict
  )

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE


type ColorScheme
  = Light
  | Dark
  | Sepia


{-| Sidebar settings of the horizontal play view, stored per song
in local storage under the `horizontalSongSettings` key.
The vertical play view may get its own styling options later.

`showPageNumbers` is `Nothing` as long as the user never toggled it,
so the default (hide page numbers for songs with up to 2 pages)
can be applied once the number of pages is known.
-}
type alias SongSettings =
  { colorScheme : ColorScheme
  , showHeading : Bool
  , showPageNumbers : Maybe Bool
  , pageMaxWidth : Float
  , centerPages : Bool
  , showDividers : Bool
  }


colorSchemeToString : ColorScheme -> String
colorSchemeToString scheme =
  case scheme of
    Light ->
      "light"
    Dark ->
      "dark"
    Sepia ->
      "sepia"


colorSchemeFromString : String -> ColorScheme
colorSchemeFromString str =
  case str of
    "dark" ->
      Dark
    "sepia" ->
      Sepia
    _ ->
      Light


decoder : Decoder SongSettings
decoder =
  JD.map6
    SongSettings
    (JD.field "colorScheme" JD.string |> JD.map colorSchemeFromString)
    (JD.field "showHeading" JD.bool)
    (JD.field "showPageNumbers" (JD.nullable JD.bool))
    (JD.field "pageMaxWidth" JD.float)
    (JD.field "centerPages" JD.bool)
    (JD.field "showDividers" JD.bool)


dictDecoder : Decoder (Dict String SongSettings)
dictDecoder =
  JD.dict decoder


encode : SongSettings -> JE.Value
encode settings =
  JE.object
    [ ( "colorScheme", JE.string (colorSchemeToString settings.colorScheme) )
    , ( "showHeading", JE.bool settings.showHeading )
    , ( "showPageNumbers"
    , settings.showPageNumbers
        |> Maybe.map JE.bool
        |> Maybe.withDefault JE.null
    )
    , ( "pageMaxWidth", JE.float settings.pageMaxWidth )
    , ( "centerPages", JE.bool settings.centerPages )
    , ( "showDividers", JE.bool settings.showDividers )
    ]


encodeDict : Dict String SongSettings -> JE.Value
encodeDict =
  JE.dict identity encode
