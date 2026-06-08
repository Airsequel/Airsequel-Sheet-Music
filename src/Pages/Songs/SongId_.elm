module Pages.Songs.SongId_ exposing (Model, Msg, page)

import Css.Global
import Effect exposing (Effect)
import GraphQL
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Layouts
import Markdown
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Tailwind.Breakpoints exposing (md)
import Tailwind.Utilities exposing (..)
import Theme exposing (Theme)
import Types.File exposing (File)
import Types.Song exposing (Song)
import Utils exposing (arrowIconVert, fileContentUrl, viewHttpError)
import View exposing (View)


page : Shared.Model -> Route { songId : String } -> Page Model Msg
page sharedModel route =
  let
    document =
      view sharedModel
  in
  Page.new
    { init = init sharedModel route
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = document
    }
    |> Page.withLayout
        (\model -> Layouts.Default
            { title = (document model).title }
        )


-- INIT
type alias Model =
  { songsResult : GraphQL.Response (List Song) }


init : Shared.Model -> Route { songId : String } -> () -> ( Model, Effect Msg )
init sharedModel route _ =
  ( { songsResult = Ok { data = Nothing, errors = Nothing } }
  , case sharedModel.readonlyId of
      Nothing ->
        Effect.none
      Just readonlyId ->
        Shared.getSongWithFiles
          readonlyId
          route.params.songId
          OnSong
  )


-- UPDATE
type Msg
  = OnSong (GraphQL.Response (List Song))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
  case msg of
    OnSong songsResult ->
      ( { model | songsResult = songsResult }
      , Effect.none
      )


-- VIEW


viewSong : Theme -> String -> Song -> Html msg
viewSong theme readOnlyId song =
  let
    keySpan value =
      span
        [ css [ inline_block, w_36, text_right, mr_2, shrink_0 ] ]
        [ text value ]

    listItem =
      li [ css [ flex ] ]

    audioFiles =
      List.filter Types.File.isAudio song.files

    sheetFiles =
      List.filter (Types.File.isAudio >> not) song.files

    hasDescription =
      case song.description of
        Just description ->
          String.trim description /= ""
        Nothing ->
          False
  in
  div
    [ css [ flex, flex_col, gap_8, md [ flex_row ] ] ]
    [ div
        [ css <|
            if hasDescription
              then [ md [ w_1over2 ] ]
              else []
        ]
        [ h2
            [ css [ text_3xl, mb_8 ] ]
            [ text
            <|
                (if song.isFavorite
                    then "⭐️ "
                    else ""
                )
                ++ song.name
            ]
        , ul
            [ css [ mb_8 ] ]
            [ listItem
                [ keySpan "Interpreter:"
                , strong
                    []
                    [ text <| Maybe.withDefault "-" song.interpreter ]
                ]
            , listItem
                [ keySpan "Composer:"
                , strong
                    []
                    [ text <| Maybe.withDefault "-" song.composer ]
                ]
            , listItem
                [ keySpan "Arranger:"
                , strong
                    []
                    [ text <| Maybe.withDefault "-" song.arranger ]
                ]
            , listItem
                [ keySpan "Instrumentation:"
                , strong
                    []
                    [ text <| Maybe.withDefault "-" song.instrumentation ]
                ]
            , listItem
                [ keySpan "Style:"
                , strong
                    []
                    [ text <| Maybe.withDefault "-" song.style ]
                ]
            , listItem
                [ keySpan "Tempo:"
                , strong
                    []
                    [ text <| Maybe.withDefault "-" song.tempo ]
                ]
            , listItem
                [ keySpan "Key:"
                , strong
                    []
                    [ text <| Maybe.withDefault "-" song.key ]
                ]
            , listItem
                [ keySpan "Number of Pages: "
                , strong
                    []
                    [ text <| String.fromInt <| List.length sheetFiles ]
                ]
            ]
        , if List.isEmpty sheetFiles
          then text ""
          else
            let
              buttonCss =
                css
                  [ inline_block
                  , border
                  , rounded
                  , px_2
                  , py_1
                  , bg_color theme.bgAccent
                  , mr_2
                  , no_underline
                  , text_color theme.textOnAccent
                  , border
                  , border_solid
                  , border_color theme.borderAccent
                  , mb_2
                  ]
            in
            div
              []
              [ if song.filetypes == Just "pdf"
                then text ""
                else a
                  [ href
                      ("/songs/horizontal/"
                        ++ String.fromInt song.rowid
                      )
                  , buttonCss
                  ]
                  [ arrowIconVert [ inline_block, w_6, h_6, rotate_90 ]
                  , span [] [ text "Horizontal View" ]
                  ]
              , a
                  [ href ("/songs/vertical/" ++ String.fromInt song.rowid)
                  , buttonCss
                  ]
                  [ arrowIconVert [ inline_block, w_6, h_6 ]
                  , span [] [ text "Vertical View" ]
                  ]
              ]
        , viewAudio readOnlyId audioFiles
        ]
    , viewDescription theme song.description
    ]


viewAudio : String -> List File -> Html msg
viewAudio readOnlyId audioFiles =
  if List.isEmpty audioFiles
    then text ""
    else
      let
        viewTrack index file =
          let
            label =
              case file.name of
                Just name ->
                  if String.trim name == ""
                    then "Audio " ++ String.fromInt (index + 1)
                    else name
                Nothing ->
                  "Audio " ++ String.fromInt (index + 1)
          in
          li
            [ css [ mb_4 ] ]
            [ p
                [ css [ mb_1, font_medium ] ]
                [ text label ]
            , audio
                [ controls True
                , preload "none"
                , src (fileContentUrl readOnlyId file.rowid)
                , css [ w_full ]
                ]
                []
            ]
      in
      div
        [ css [ mt_8 ] ]
        [ h3
            [ css [ text_xl, mb_4 ] ]
            [ text "Audio" ]
        , ul [] (List.indexedMap viewTrack audioFiles)
        ]


viewDescription : Theme -> Maybe String -> Html msg
viewDescription theme maybeDescription =
  case maybeDescription of
    Just description ->
      if String.trim description == ""
        then text ""
        else div
          [ css
              [ md [ w_1over2 ]
              , Css.Global.descendants
                  [ Css.Global.typeSelector
                      "h1"
                      [ text_2xl, font_bold, mt_4, mb_3 ]
                  , Css.Global.typeSelector
                      "h2"
                      [ text_xl, font_bold, mt_4, mb_2 ]
                  , Css.Global.typeSelector
                      "h3"
                      [ text_lg, font_bold, mt_3, mb_2 ]
                  , Css.Global.typeSelector
                      "p"
                      [ mb_4, leading_relaxed ]
                  , Css.Global.typeSelector
                      "ul"
                      [ list_disc, pl_6, mb_4 ]
                  , Css.Global.typeSelector
                      "ol"
                      [ list_decimal, pl_6, mb_4 ]
                  , Css.Global.typeSelector "li" [ mb_1 ]
                  , Css.Global.typeSelector
                      "a"
                      [ text_color theme.textLink, underline ]
                  , Css.Global.typeSelector
                      "code"
                      [ font_mono, px_1, rounded, bg_color theme.bgRowAlt ]
                  , Css.Global.typeSelector
                      "pre"
                      [ font_mono
                      , p_3
                      , rounded
                      , overflow_x_auto
                      , bg_color theme.bgRowAlt
                      , mb_4
                      ]
                  , Css.Global.typeSelector
                      "blockquote"
                      [ border_l_4
                      , border_color theme.border
                      , pl_4
                      , italic
                      , text_color theme.textSecondary
                      ]
                  , Css.Global.typeSelector "img" [ max_w_full ]
                  ]
              ]
          ]
          [ Html.Styled.fromUnstyled (Markdown.toHtml [] description) ]
    Nothing ->
      text ""


view : Shared.Model -> Model -> View Msg
view sharedModel model =
  case model.songsResult of
    Ok gqlRes ->
      case gqlRes.data of
        Just songs ->
          case songs.root of
            song :: _ ->
              let
                theme =
                  Theme.fromDarkMode (Shared.Model.isDark sharedModel)
              in
              { title = song.name
              , body = [ toUnstyled <|
                    div
                      []
                      [ viewSong
                          theme
                          (Maybe.withDefault
                              ""
                              sharedModel.readonlyId
                          )
                          song
                      ]
                ]
              }
            _ ->
              { title = "Error: Multiple songs"
              , body = [ Html.text "Error: Multiple songs" ]
              }
        Nothing ->
          { title = "Loading …"
          , body = [ Html.text "Loading …" ]
          }
    Err error ->
      { title = "Error"
      , body = [ toUnstyled <| viewHttpError error ]
      }
