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

import Dict exposing (Dict)
import Effect exposing (Effect)
import GraphQL
import Json.Decode
import Ports
import Process
import Route exposing (Route)
import Shared.Model exposing (ColorPref(..))
import Shared.Msg
import Task
import Types.FilterOptions exposing (filterOptionsDecoder)
import Types.Song exposing (songsDecoder, songsPageDecoder)
import Types.SongSettings exposing (SongSettings)
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


{-| Escape a string for embedding in a GraphQL string literal.
-}
gqlEscape : String -> String
gqlEscape =
  String.replace "\\" "\\\\" >> String.replace "\"" "\\\""


{-| Build the GraphQL `filter` argument from the search string and the
dropdown filters. Conditions on different columns are combined with AND
by the API. The instrumentation multi-select means "song contains ALL
selected items": the view's `instrumentation_normalized` column holds
the items sorted and double-comma-delimited (e.g. `,,Guitar,,Piano,,`),
so a single `like` pattern with the selections in the same sort order
(e.g. `%,Guitar,%,Piano,%`) expresses the conjunction.
-}
buildFilterArg : Maybe String -> Shared.Model.Filters -> String
buildFilterArg searchMb filters =
  let
    eqPart column valueMb =
      valueMb
        |> Maybe.map
            (\value -> column ++ ": { eq: \"" ++ gqlEscape value ++ "\" }"
            )

    searchPart =
      searchMb
        |> Maybe.map
            (\search -> "search_text: { like: \"%"
              ++ gqlEscape (String.toLower search)
              ++ "%\" }"
            )

    instrumentationPart =
      if List.isEmpty filters.instrumentation
        then Nothing
        else Just
          ("instrumentation_normalized: { like: \"%"
            ++ (filters.instrumentation
              |> List.map String.trim
              |> List.sort
              |> List.map (\item -> "," ++ gqlEscape item ++ ",")
              |> String.join "%"
            )
            ++ "%\" }"
          )

    parts =
      List.filterMap
        identity
        [ searchPart
        , eqPart "interpreter" filters.interpreter
        , instrumentationPart
        , eqPart "key" filters.key
        , eqPart "tempo" filters.tempo
        ]
  in
  if List.isEmpty parts
    then ""
    else "filter: { " ++ String.join ", " parts ++ " }"


{-| Fetch one page of songs (server-side pagination via limit/offset).
The `songs_paginated_json` view includes the overall number of songs as
`total_count`, a lowercased `search_text` column (interpreter, name,
instrumentation, key) so a single `like` filter searches across all
those columns at once, and `instrumentation_normalized` for the
instrumentation filter.
The explicit `order_by` (favorites first) is required for stable
pagination — SQLite does not guarantee the ORDER BY inside a view
once an outer filter is applied.
One extra row is requested to detect whether a next page exists
(`total_count` ignores the filter, so it cannot be used while
searching or filtering); the extra row is trimmed off again
in the `OnSongs` handler.
-}
getSongs : String -> Maybe String -> Shared.Model.Filters -> Int -> Effect Msg
getSongs readonlyId searchMb filters page =
  let
    filterArg =
      buildFilterArg searchMb filters
  in
  Effect.sendCmd <|
    GraphQL.run
      { query = """
                query Songs {
                    songs_paginated_json (
                        """
        ++ filterArg
        ++ """
                        order_by: [{ is_favorite: DESC }, { rowid: ASC }]
                        limit: """
        ++ String.fromInt (Shared.Model.songsPerPage + 1)
        ++ """
                        offset: """
        ++ String.fromInt ((page - 1) * Shared.Model.songsPerPage)
        ++ """
                    ) {
                        rowid
                        name
                        instrumentation
                        style
                        tempo
                        key
                        interpreter
                        composer
                        arranger
                        numberOfFiles
                        filetypes
                        is_favorite
                        total_count
                    }
                }
                """
      , decoder = songsPageDecoder
      , root = "songs_paginated_json"
      , url = host
        ++ "/readonly/"
        ++ readonlyId
        ++ "/graphql"
      , headers = []
      , on = Shared.Msg.OnSongs
      , variables = Nothing
      }


{-| Fetch the distinct filter values of the whole collection
(for the filter dropdowns), provided by the `filter_options_json` view.
-}
getFilterOptions : String -> Effect Msg
getFilterOptions readonlyId =
  Effect.sendCmd <|
    GraphQL.run
      { query = """
                query FilterOptions {
                    filter_options_json {
                        field
                        value
                    }
                }
                """
      , decoder = filterOptionsDecoder
      , root = "filter_options_json"
      , url = host
        ++ "/readonly/"
        ++ readonlyId
        ++ "/graphql"
      , headers = []
      , on = Shared.Msg.OnFilterOptions
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
                    style
                    tempo
                    key
                    interpreter
                    composer
                    arranger
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
  , horizontalSongSettings : Dict String SongSettings
  }


decoder : Json.Decode.Decoder Flags
decoder =
  Json.Decode.map4
    Flags
    (Json.Decode.field "readonlyId" (Json.Decode.maybe Json.Decode.string))
    (Json.Decode.field "colorPref" Json.Decode.string
      |> Json.Decode.map colorPrefFromString
    )
    (Json.Decode.field "systemDark" Json.Decode.bool)
    -- Fall back to no stored settings if the data is missing or corrupt,
    -- so the other flags still get applied
    (Json.Decode.oneOf
        [ Json.Decode.field "horizontalSongSettings" Types.SongSettings.dictDecoder
        , Json.Decode.succeed Dict.empty
        ]
    )


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
      , songsLoading = False
      , songsPage = 1
      , songsSearch = Nothing
      , songsSearchVersion = 0
      , songsFilters = Shared.Model.emptyFilters
      , hasNextSongsPage = False
      , filterOptions = Nothing
      , colorPref = Auto
      , systemDark = False
      , horizontalSongSettings = Dict.empty
      }
  in
  case flagsResult of
    Ok flags ->
      let
        themedModel =
          { emptyModel
            | colorPref = flags.colorPref
            , systemDark = flags.systemDark
            , horizontalSongSettings = flags.horizontalSongSettings
          }
      in
      case flags.readonlyId of
        Just readonlyId ->
          ( { themedModel | readonlyId = Just readonlyId }
          , Effect.batch
              [ getSongs readonlyId Nothing Shared.Model.emptyFilters 1
              , getFilterOptions readonlyId
              ]
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
          , songsLoading = True
          , songsPage = 1
          , songsSearch = Nothing
          , songsFilters = Shared.Model.emptyFilters
          , filterOptions = Nothing
        }
      , Effect.batch
          [ Effect.saveReadonlyId readonlyId
          , getSongs readonlyId Nothing Shared.Model.emptyFilters 1
          , getFilterOptions readonlyId
          ]
      )
    Shared.Msg.SelectedSongsPage page ->
      case model.readonlyId of
        Just readonlyId ->
          ( { model
              | songsPage = page
              , songsLoading = True
            }
          , getSongs readonlyId model.songsSearch model.songsFilters page
          )
        Nothing ->
          ( model, Effect.none )
    Shared.Msg.SetSongsFilters filters ->
      case model.readonlyId of
        Just readonlyId ->
          ( { model
              | songsFilters = filters
              , songsPage = 1
              , songsLoading = True
            }
          , getSongs readonlyId model.songsSearch filters 1
          )
        Nothing ->
          ( { model | songsFilters = filters }, Effect.none )
    Shared.Msg.ResetSongsSearchAndFilters ->
      let
        -- Invalidate any pending search debounce
        newModel =
          { model
            | songsSearch = Nothing
            , songsSearchVersion = model.songsSearchVersion + 1
            , songsFilters = Shared.Model.emptyFilters
            , songsPage = 1
          }
      in
      case model.readonlyId of
        Just readonlyId ->
          ( { newModel | songsLoading = True }
          , getSongs readonlyId Nothing Shared.Model.emptyFilters 1
          )
        Nothing ->
          ( newModel, Effect.none )
    Shared.Msg.EnteredSongsSearch searchStr ->
      let
        newVersion =
          model.songsSearchVersion + 1

        searchMb =
          if String.isEmpty (String.trim searchStr)
            then Nothing
            else Just searchStr
      in
      ( { model
          | songsSearch = searchMb
          , songsSearchVersion = newVersion
          , songsPage = 1
        }
      , Effect.sendCmd <|
          Task.perform
            (\_ -> Shared.Msg.DebouncedSongsSearch newVersion)
            (Process.sleep 350)
      )
    Shared.Msg.DebouncedSongsSearch version ->
      -- Only fetch if no further keystroke arrived during the delay.
      -- Only the table body shows a loading state;
      -- the rest of the page stays in place.
      if version == model.songsSearchVersion
        then case model.readonlyId of
          Just readonlyId ->
            ( { model | songsLoading = True }
            , getSongs readonlyId model.songsSearch model.songsFilters 1
            )
          Nothing ->
            ( model, Effect.none )
        else ( model, Effect.none )
    Shared.Msg.OnSongs songsResult ->
      let
        numberOfLoadedSongs =
          case songsResult of
            Ok res ->
              res.data
                |> Maybe.map (.root >> .songs >> List.length)
                |> Maybe.withDefault 0
            Err _ ->
              0

        -- Drop the extra row that was only fetched
        -- to detect whether a next page exists
        trimmedResult =
          songsResult
            |> Result.map
                (\res -> { res
                    | data = res.data
                        |> Maybe.map
                            (\d ->
                                let
                                  root =
                                    d.root
                                in
                                { d
                                  | root = { root
                                      | songs = List.take
                                          Shared.Model.songsPerPage
                                          root.songs
                                    }
                                }
                            )
                  }
                )
      in
      ( { model
          | songsResult = trimmedResult
          , songsLoading = False
          , hasNextSongsPage = numberOfLoadedSongs > Shared.Model.songsPerPage
        }
      , Effect.none
      )
    Shared.Msg.OnFilterOptions optionsResult ->
      -- On failure (e.g. a database without the `filter_options_json`
      -- view yet), the dropdowns fall back to the values
      -- of the currently loaded page
      case optionsResult of
        Ok res ->
          ( { model | filterOptions = res.data |> Maybe.map .root }
          , Effect.none
          )
        Err _ ->
          ( model, Effect.none )
    Shared.Msg.SetColorPref pref ->
      ( { model | colorPref = pref }
      , Effect.saveColorPref (colorPrefToString pref)
      )
    Shared.Msg.SystemDarkChanged isDark ->
      ( { model | systemDark = isDark }
      , Effect.none
      )
    Shared.Msg.SetHorizontalSongSettings songId settings ->
      let
        newHorizontalSongSettings =
          Dict.insert songId settings model.horizontalSongSettings
      in
      ( { model | horizontalSongSettings = newHorizontalSongSettings }
      , Effect.saveHorizontalSongSettings newHorizontalSongSettings
      )


-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
  Ports.systemDarkChanged Shared.Msg.SystemDarkChanged
