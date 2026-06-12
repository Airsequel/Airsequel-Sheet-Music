module Shared.Model exposing
  ( ColorPref(..)
  , Filters
  , Model
  , emptyFilters
  , filtersActive
  , isDark
  , songsPerPage
  )

import Dict exposing (Dict)
import GraphQL
import Types.FilterOptions exposing (FilterOptions)
import Types.Song exposing (SongsPage)
import Types.SongSettings exposing (SongSettings)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type ColorPref
  = Auto
  | Light
  | Dark


type alias Model =
  { readonlyId : Maybe String
  , songsResult : GraphQL.Response SongsPage
  , songsLoading : Bool
  , songsPage : Int
  , songsSearch : Maybe String
  , songsSearchVersion : Int
  , songsFilters : Filters
  , hasNextSongsPage : Bool
  , filterOptions : Maybe FilterOptions
  , colorPref : ColorPref
  , systemDark : Bool
  , horizontalSongSettings : Dict String SongSettings
  }


{-| Active dropdown filters; applied server-side
via the GraphQL `filter` argument.
-}
type alias Filters =
  { interpreter : Maybe String
  , instrumentation : List String
  , key : Maybe String
  , tempo : Maybe String
  }


emptyFilters : Filters
emptyFilters =
  { interpreter = Nothing
  , instrumentation = []
  , key = Nothing
  , tempo = Nothing
  }


filtersActive : Filters -> Bool
filtersActive f =
  f.interpreter
  /= Nothing
  || not (List.isEmpty f.instrumentation)
  || f.key
  /= Nothing
  || f.tempo
  /= Nothing


{-| Number of songs fetched per page (server-side via limit/offset).
-}
songsPerPage : Int
songsPerPage =
  50


{-| Resolve the effective dark-mode flag from the user's preference
and the current system setting.
-}
isDark : Model -> Bool
isDark model =
  case model.colorPref of
    Auto ->
      model.systemDark
    Light ->
      False
    Dark ->
      True
