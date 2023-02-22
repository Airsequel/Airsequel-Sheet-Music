module Shared.Model exposing (Model)

import GraphQL
import Types.Song exposing (Song)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type alias Model =
    { readonlyId : Maybe String
    , songsResult : GraphQL.Response (List Song)
    }
