module Layouts.Default exposing
  ( Model
  , Msg
  , Props
  , layout
  )

import Css
import Css.Global
import Effect exposing (Effect)
import Html.Styled exposing (a, button, div, h1, main_, nav, span, text, toUnstyled)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Layout exposing (Layout)
import Route exposing (Route)
import Shared
import Shared.Model exposing (ColorPref(..))
import Shared.Msg
import Tailwind.Breakpoints exposing (..)
import Tailwind.Utilities exposing (..)
import Theme exposing (Theme)
import View exposing (View)


type alias Props =
  { title : String }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout settings sharedModel _ =
  Layout.new
    { init = init
    , update = update
    , view = view settings sharedModel
    , subscriptions = subscriptions
    }


-- MODEL
type alias Model =
  {}


init : () -> ( Model, Effect Msg )
init _ =
  ( {}
  , Effect.none
  )


-- UPDATE
type Msg
  = SetColorPref ColorPref


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
  case msg of
    SetColorPref pref ->
      ( model
      , Effect.sendSharedMsg (Shared.Msg.SetColorPref pref)
      )


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- VIEW


colorPrefControl : Theme -> ColorPref -> Html.Styled.Html Msg
colorPrefControl theme current =
  let
    btn pref label hint =
      let
        selected =
          pref == current
      in
      button
        [ onClick (SetColorPref pref)
        , title hint
        , css <|
            [ cursor_pointer
            , inline_block
            , w_8
            , h_8
            , border
            , border_solid
            , border_color theme.border
            , text_color theme.textPrimary
            , text_lg
            , leading_none
            , Css.hover [ bg_color theme.bgRowAlt ]
            ]
            ++ (if selected
                  then [ bg_color theme.bgAccentSoft ]
                  else [ bg_color theme.bgPanel ]
              )
        ]
        [ span [ css [ relative ] ] [ text label ] ]
  in
  div
    [ css [ inline_flex, rounded, overflow_hidden ] ]
    [ btn Light "☀" "Light mode"
    , btn Auto "◐" "Match system preference"
    , btn Dark "☾" "Dark mode"
    ]


view :
  Props
  -> Shared.Model
  -> { toContentMsg : Msg -> mainMsg
  , content : View mainMsg
  , model : Model
  }
  -> View mainMsg
view settings sharedModel { toContentMsg, content } =
  let
    darkMode =
      Shared.Model.isDark sharedModel

    theme =
      Theme.fromDarkMode darkMode
  in
  { title = settings.title
  , body = [ toUnstyled <|
        main_
          [ css
              [ bg_color theme.bgPanel
              , py_12
              , px_10
              , max_w_5xl
              , mx_auto
              , min_h_full
              , text_color theme.textPrimary
              , border_color theme.border
              , lg [ border_x ]
              ]
          ]
          [ Css.Global.global globalStyles
          , Css.Global.global (Theme.globalSnippets darkMode)
          , nav
              [ css
                  [ flex
                  , flex_col
                  , sm [ flex_row ]
                  , items_center
                  , gap_3
                  , pb_8
                  ]
              ]
              [ h1
                  [ css
                      [ font_bold
                      , text_3xl
                      , mr_4
                      , inline_block
                      , text_color theme.textLink
                      , grow
                      ]
                  ]
                  [ a
                      [ href "/" ]
                      [ text "Airsequel Sheet Music" ]
                  ]
              , Html.Styled.map toContentMsg <|
                  colorPrefControl theme sharedModel.colorPref
              ]
          , div
              []
              (content.body
                |> List.map Html.Styled.fromUnstyled
              )
          ]
    ]
  }
