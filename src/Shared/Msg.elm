module Shared.Msg exposing (Msg(..))

import GraphQL
import Shared.Model exposing (ColorPref)
import Types.Song exposing (Song)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type Msg
  = OnSongs (GraphQL.Response (List Song))
  | SubmittedReadonlyId String
  | SetColorPref ColorPref
  | SystemDarkChanged Bool
