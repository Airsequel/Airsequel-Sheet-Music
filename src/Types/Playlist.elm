module Types.Playlist exposing
  ( Playlist
  , playlistsDecoder
  )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP


type alias Playlist =
  { rowid : Int
  , name : String
  , songIds : List Int
  , hasCoverImage : Bool
  }


{-| The `songs` column is a JSON array of song rowids, stored as text
(and sometimes containing embedded newlines from manual editing).
`Json.Decode.decodeString` tolerates the whitespace; a malformed value
falls back to an empty list so the rest of the playlist still renders.
-}
songIdsDecoder : Decoder (List Int)
songIdsDecoder =
  JD.field "songs" JD.string
    |> JD.map
        (\songsJson -> JD.decodeString (JD.list JD.int) songsJson
            |> Result.withDefault []
        )


{-| The `cover_image` BLOB_FILE column is returned as a JSON string like
`{"url":"…"}` when an image is set, or `null` when it is not. The URL it
contains embeds the private database ID and is not publicly reachable, so
we only record whether a cover exists; the displayable URL is built from
the read-only ID via `Utils.columnFileUrl`.
-}
hasCoverImageDecoder : Decoder Bool
hasCoverImageDecoder =
  JD.field "cover_image" (JD.nullable JD.string)
    |> JD.map (\coverMb -> coverMb /= Nothing)


playlistDecoder : Decoder Playlist
playlistDecoder =
  JD.succeed Playlist
    |> JDP.required "rowid" JD.int
    |> JDP.custom
        (JD.field "name" (JD.nullable JD.string)
          |> JD.map (Maybe.withDefault "")
        )
    |> JDP.custom songIdsDecoder
    |> JDP.custom hasCoverImageDecoder


playlistsDecoder : Decoder (List Playlist)
playlistsDecoder =
  JD.list playlistDecoder
