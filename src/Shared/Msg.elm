module Shared.Msg exposing (Msg(..))

import GraphQL
import Shared.Model exposing (ColorPref, Filters)
import Types.FilterOptions exposing (FilterOptions)
import Types.Song exposing (SongsPage)
import Types.SongSettings exposing (SongSettings)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type Msg
  = OnSongs (GraphQL.Response SongsPage)
  | OnFilterOptions (GraphQL.Response FilterOptions)
  | SubmittedReadonlyId String
  | SelectedSongsPage Int
  | EnteredSongsSearch String
  | DebouncedSongsSearch Int
  | SetSongsFilters Filters
  | ResetSongsSearchAndFilters
  | SetColorPref ColorPref
  | SystemDarkChanged Bool
  | SetHorizontalSongSettings String SongSettings
