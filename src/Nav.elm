module Nav exposing (Tab(..), viewTabs)

{-| Shared tab bar switching between the songs list (`/`) and the
playlists list (`/playlists`). Rendered inside the header of both the
home page and the default layout.

@docs Tab, viewTabs

-}

import Css
import Html.Styled exposing (Html, a, div, text)
import Html.Styled.Attributes exposing (css, href)
import Tailwind.Utilities exposing (..)
import Theme exposing (Theme)


type Tab
  = SongsTab
  | PlaylistsTab


viewTabs : Theme -> Tab -> Html msg
viewTabs theme active =
  let
    tab label path isActive =
      a
        [ href path
        , css
            ([ inline_block
              , px_4
              , py_1_dot_5
              , rounded
              , no_underline
              , text_sm
              , font_medium
              ]
              ++ (if isActive
                  then [ bg_color theme.bgAccent
                  , text_color theme.textOnAccent
                  , font_semibold
                  ]
                  else [ text_color theme.textLink
                  , Css.hover [ bg_color theme.bgRowAlt ]
                  ]
              )
            )
        ]
        [ text label ]
  in
  div
    [ css [ flex, gap_2 ] ]
    [ tab "Songs" "/" (active == SongsTab)
    , tab "Playlists" "/playlists" (active == PlaylistsTab)
    ]
