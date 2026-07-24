module Pages.Playlists exposing (Model, Msg, page)

import Css
import Effect exposing (Effect)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Tailwind.Breakpoints exposing (..)
import Tailwind.Utilities exposing (..)
import Theme exposing (Theme)
import Types.Playlist exposing (Playlist)
import Utils exposing (columnFileUrl, viewGraphQLErrors, viewHttpError)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page sharedModel _ =
  Page.new
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view sharedModel
    }
    |> Page.withLayout
        (\_ -> Layouts.Default { title = "Playlists" })


-- INIT
type alias Model =
  {}


init : () -> ( Model, Effect Msg )
init () =
  ( {}, Effect.none )


-- UPDATE
type Msg
  = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Effect.none )


-- VIEW


viewPlaylist : Theme -> String -> Playlist -> Html Msg
viewPlaylist theme readonlyId playlist =
  let
    songCount =
      List.length playlist.songIds

    cover =
      if playlist.hasCoverImage
        then img
          [ src
              (columnFileUrl
                  readonlyId
                  "playlists"
                  "cover_image"
                  playlist.rowid
              )
          , alt ("Cover of " ++ playlist.name)
          , css
              [ w_full
              , Css.property "aspect-ratio" "1 / 1"
              , object_cover
              , block
              ]
          ]
          []
        else div
          [ css
              [ w_full
              , Css.property "aspect-ratio" "1 / 1"
              , flex
              , items_center
              , justify_center
              , bg_color theme.bgAccentMuted
              , text_color theme.textMuted
              , text_4xl
              ]
          ]
          [ text "♪" ]
  in
  a
    [ href ("/playlists/" ++ String.fromInt playlist.rowid)
    , css
        [ block
        , no_underline
        , border
        , border_solid
        , border_color theme.borderMuted
        , rounded_lg
        , overflow_hidden
        , bg_color theme.bgPanel
        , Css.hover [ border_color theme.borderAccent ]
        ]
    ]
    [ cover
    , div
        [ css [ p_3 ] ]
        [ h2
            [ css [ font_semibold, text_color theme.textLink ] ]
            [ text playlist.name ]
        , p
            [ css [ text_sm, text_color theme.textMuted, mt_1 ] ]
            [ text
                (String.fromInt songCount
                  ++ (if songCount == 1
                      then " song"
                      else " songs"
                  )
                )
            ]
        ]
    ]


viewPlaylists : Theme -> String -> List Playlist -> Html Msg
viewPlaylists theme readonlyId playlists =
  if List.isEmpty playlists
    then p
      [ css [ text_color theme.textMuted, py_8 ] ]
      [ text "No playlists yet." ]
    else div
      [ css
          [ grid
          , grid_cols_2
          , sm [ grid_cols_3 ]
          , md [ grid_cols_4 ]
          , gap_4
          ]
      ]
      (List.map (viewPlaylist theme readonlyId) playlists)


view : Shared.Model -> Model -> View Msg
view sharedModel _ =
  let
    darkMode =
      Shared.Model.isDark sharedModel

    theme =
      Theme.fromDarkMode darkMode

    readonlyIdEmpty =
      (sharedModel.readonlyId == Nothing)
      || (sharedModel.readonlyId == Just "")

    body =
      if readonlyIdEmpty
        then [ p
            [ css [ text_color theme.textMuted, py_8 ] ]
            [ text "Set a read-only database ID on the "
            , a
                [ href "/"
                , css [ underline, text_color theme.textLink ]
                ]
                [ text "songs page" ]
            , text " to load playlists."
            ]
        ]
        else case sharedModel.playlistsResult of
          Ok gqlRes ->
            case gqlRes.data of
              Just playlistsData ->
                [ viewPlaylists
                    theme
                    (Maybe.withDefault "" sharedModel.readonlyId)
                    playlistsData.root
                ]
              Nothing ->
                case gqlRes.errors of
                  Just gqlErrors ->
                    [ viewGraphQLErrors gqlErrors ]
                  Nothing ->
                    [ div
                        [ css [ text_center, py_8, text_color theme.textMuted ] ]
                        [ text "Loading …" ]
                    ]
          Err httpError ->
            [ viewHttpError httpError ]
  in
  { title = "Playlists — Airsequel Sheet Music"
  , body = [ toUnstyled <|
        div [ css [ text_color theme.textPrimary ] ] body
    ]
  }
