module Shared exposing
  ( Flags
  , decoder
  , Model
  , Msg
  , init
  , update
  , subscriptions
  , getSongWithFiles
  )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions
@docs getSongWithFiles

-}

import Effect exposing (Effect)
import GraphQL
import Json.Decode
import Ports
import Route exposing (Route)
import Shared.Model exposing (ColorPref(..))
import Shared.Msg
import Types.Song exposing (songsDecoder)
import Utils exposing (host)


colorPrefFromString : String -> ColorPref
colorPrefFromString str =
  case str of
    "light" ->
      Light
    "dark" ->
      Dark
    _ ->
      Auto


colorPrefToString : ColorPref -> String
colorPrefToString pref =
  case pref of
    Auto ->
      "auto"
    Light ->
      "light"
    Dark ->
      "dark"


getSongs : String -> Effect Msg
getSongs readonlyId =
  Effect.sendCmd <|
    GraphQL.run
      { query = """
                query Songs {
                    songs_json {
                        rowid
                        name
                        instrumentation
                        tempo
                        key
                        interpreter
                        numberOfFiles
                        filetypes
                        is_favorite
                    }
                }
                """
      , decoder = songsDecoder False
      , root = "songs_json"
      , url = host
        ++ "/readonly/"
        ++ readonlyId
        ++ "/graphql"
      , headers = []
      , on = Shared.Msg.OnSongs
      , variables = Nothing
      }


getSongWithFiles :
  String
  -> String
  -> (GraphQL.Response (List Types.Song.Song) -> msg)
  -> Effect msg
getSongWithFiles readonlyId songId msg =
  Effect.sendCmd <|
    GraphQL.run
      { query = """
            query SongsWithFiles {
                songs_with_files_json (
                    filter: { rowid: { eq: """
        ++ songId
        ++ """ } }
                ) {
                    rowid
                    name
                    instrumentation
                    tempo
                    key
                    interpreter
                    description
                    numberOfFiles
                    filetypes
                    files
                    is_favorite
                }
            }
            """
      , decoder = songsDecoder True
      , root = "songs_with_files_json"
      , url = host
        ++ "/readonly/"
        ++ readonlyId
        ++ "/graphql"
      , headers = []
      , on = msg
      , variables = Nothing
      }


-- FLAGS
type alias Flags =
  { readonlyId : Maybe String
  , colorPref : ColorPref
  , systemDark : Bool
  }


decoder : Json.Decode.Decoder Flags
decoder =
  Json.Decode.map3
    Flags
    (Json.Decode.field "readonlyId" (Json.Decode.maybe Json.Decode.string))
    (Json.Decode.field "colorPref" Json.Decode.string
      |> Json.Decode.map colorPrefFromString
    )
    (Json.Decode.field "systemDark" Json.Decode.bool)


-- INIT
type alias Model =
  Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult _ =
  let
    emptyModel =
      { readonlyId = Nothing
      , songsResult = Ok
          { data = Nothing
          , errors = Nothing
          }
      , colorPref = Auto
      , systemDark = False
      }
  in
  case flagsResult of
    Ok flags ->
      let
        themedModel =
          { emptyModel
            | colorPref = flags.colorPref
            , systemDark = flags.systemDark
          }
      in
      case flags.readonlyId of
        Just readonlyId ->
          ( { themedModel | readonlyId = Just readonlyId }
          , getSongs readonlyId
          )
        Nothing ->
          ( themedModel, Effect.none )
    Err _ ->
      ( emptyModel, Effect.none )


-- UPDATE
type alias Msg =
  Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
  case msg of
    Shared.Msg.SubmittedReadonlyId readonlyId ->
      ( { model
          | readonlyId = Just readonlyId
          , songsResult = Ok { data = Nothing, errors = Nothing }
        }
      , Effect.batch
          [ Effect.saveReadonlyId readonlyId
          , getSongs readonlyId
          ]
      )
    Shared.Msg.OnSongs songsResult ->
      ( { model | songsResult = songsResult }
      , Effect.none
      )
    Shared.Msg.SetColorPref pref ->
      ( { model | colorPref = pref }
      , Effect.saveColorPref (colorPrefToString pref)
      )
    Shared.Msg.SystemDarkChanged isDark ->
      ( { model | systemDark = isDark }
      , Effect.none
      )


-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
  Ports.systemDarkChanged Shared.Msg.SystemDarkChanged
