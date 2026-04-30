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
  { bgPage = gray_900
  , bgPanel = gray_800
  , bgRowAlt = gray_700
  , bgInput = gray_700
  , bgButton = blue_900
  , bgButtonHover = blue_800
  , bgButtonActive = blue_700
  , bgAccentSoft = blue_900
  , bgAccent = blue_800
  , bgAccentMuted = blue_900
  , bgError = red_900
  , textPrimary = gray_100
  , textSecondary = gray_300
  , textMuted = gray_400
  , textPlaceholder = gray_500
  , textLink = blue_300
  , textError = red_300
  , border = gray_600
  , borderMuted = gray_700
  , borderAccent = blue_300
  , borderError = red_300
  , sidebarBg = gray_800
  , sidebarBtn = gray_700
  , sidebarBtnHover = gray_600
  , sidebarBtnActive = gray_500
  , sidebarBtnSelected = gray_500
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
  , Css.Global.selector "tr:nth-child(even)"
      [ Css.backgroundColor <|
          if darkMode
            then Css.rgb 42 42 42
            else Css.rgb 242 242 242
      ]
  ]
