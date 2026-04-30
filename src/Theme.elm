module Theme exposing (Theme, fromDarkMode, globalSnippets)

{-|

@docs Theme, fromDarkMode, globalSnippets

-}

import Css
import Css.Global
import Tailwind.Theme exposing (..)


{-| Theme colors keyed by purpose so views don't pick literal palette swatches.
-}
type alias Theme =
  { bgPage : Color
  , bgPanel : Color
  , bgRowAlt : Color
  , bgInput : Color
  , bgButton : Color
  , bgButtonHover : Color
  , bgButtonActive : Color
  , bgAccentSoft : Color
  , bgAccent : Color
  , bgAccentMuted : Color
  , bgError : Color
  , textPrimary : Color
  , textSecondary : Color
  , textMuted : Color
  , textPlaceholder : Color
  , textLink : Color
  , textError : Color
  , border : Color
  , borderMuted : Color
  , borderAccent : Color
  , borderError : Color
  , sidebarBg : Color
  , sidebarBtn : Color
  , sidebarBtnHover : Color
  , sidebarBtnActive : Color
  , sidebarBtnSelected : Color
  }


light : Theme
light =
  { bgPage = white
  , bgPanel = white
  , bgRowAlt = gray_100
  , bgInput = white
  , bgButton = blue_200
  , bgButtonHover = blue_100
  , bgButtonActive = blue_300
  , bgAccentSoft = blue_100
  , bgAccent = blue_200
  , bgAccentMuted = blue_100
  , bgError = red_200
  , textPrimary = black
  , textSecondary = gray_700
  , textMuted = gray_500
  , textPlaceholder = gray_400
  , textLink = blue_800
  , textError = red_800
  , border = gray_400
  , borderMuted = gray_300
  , borderAccent = blue_800
  , borderError = red_800
  , sidebarBg = gray_200
  , sidebarBtn = gray_100
  , sidebarBtnHover = gray_50
  , sidebarBtnActive = gray_200
  , sidebarBtnSelected = gray_300
  }


dark : Theme
dark =
  { bgPage = neutral_900
  , bgPanel = neutral_800
  , bgRowAlt = neutral_700
  , bgInput = neutral_700
  , bgButton = neutral_700
  , bgButtonHover = neutral_600
  , bgButtonActive = neutral_500
  , bgAccentSoft = neutral_700
  , bgAccent = neutral_700
  , bgAccentMuted = neutral_700
  , bgError = red_900
  , textPrimary = neutral_100
  , textSecondary = neutral_300
  , textMuted = neutral_400
  , textPlaceholder = neutral_500
  , textLink = neutral_100
  , textError = red_300
  , border = neutral_600
  , borderMuted = neutral_700
  , borderAccent = neutral_300
  , borderError = red_300
  , sidebarBg = neutral_800
  , sidebarBtn = neutral_700
  , sidebarBtnHover = neutral_600
  , sidebarBtnActive = neutral_500
  , sidebarBtnSelected = neutral_500
  }


fromDarkMode : Bool -> Theme
fromDarkMode darkMode =
  if darkMode
    then dark
    else light


{-| Global CSS snippets that need to react to dark mode
(body background, alternating table rows etc.).
-}
globalSnippets : Bool -> List Css.Global.Snippet
globalSnippets darkMode =
  [ Css.Global.body
      [ Css.backgroundColor <|
          if darkMode
            then Css.hsl 0 0 0.1
            else Css.hsl 0 0 0.95
      ]
  , Css.Global.selector
      "tr:nth-child(even)"
      [ Css.backgroundColor <|
          if darkMode
            then Css.rgb 42 42 42
            else Css.rgb 242 242 242
      ]
  ]
