port module Ports exposing (systemDarkChanged)

{-|

@docs systemDarkChanged

-}

{-| Notifies Elm whenever the OS-level
`prefers-color-scheme: dark` media query changes.
-}
port systemDarkChanged : (Bool -> msg) -> Sub msg
