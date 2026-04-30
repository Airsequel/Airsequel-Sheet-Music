module Shared.Model exposing (ColorPref(..), Model, isDark)

import GraphQL
import Types.Song exposing (Song)


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
  , songsResult : GraphQL.Response (List Song)
  , colorPref : ColorPref
  , systemDark : Bool
  }


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
