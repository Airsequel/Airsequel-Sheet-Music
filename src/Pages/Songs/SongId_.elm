module Pages.Songs.SongId_ exposing (Model, Msg, page)

import Bytes exposing (Bytes)
import Css.Global
import Effect exposing (Effect)
import File.Download as Download
import GraphQL
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Http
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
import Utils exposing (arrowIconVert, fileContentUrl, viewGraphQLErrors, viewHttpError)
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
  | DownloadFile String String -- filename, url
  | GotFileBytes String (Result Http.Error Bytes) -- filename, content



update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
  case msg of
    OnSong songsResult ->
      ( { model | songsResult = songsResult }
      , Effect.none
      )
    DownloadFile filename url ->
      -- Fetch the bytes ourselves and offer them as a blob download.
      -- The plain `download` attribute is ignored for cross-origin URLs,
      -- so the browser would otherwise name the file from the sniffed
      -- content type (e.g. an .mxl saved as .zip).
      ( model
      , Effect.sendCmd
          (Http.get
              { url = url
              , expect = Http.expectBytesResponse
                  (GotFileBytes filename)
                  bytesResponse
              }
          )
      )
    GotFileBytes filename (Ok bytes) ->
      ( model
      , Effect.sendCmd
          (Download.bytes filename "application/octet-stream" bytes)
      )
    GotFileBytes _ (Err _) ->
      ( model, Effect.none )


bytesResponse : Http.Response Bytes -> Result Http.Error Bytes
bytesResponse response =
  case response of
    Http.BadUrl_ url ->
      Err (Http.BadUrl url)
    Http.Timeout_ ->
      Err Http.Timeout
    Http.NetworkError_ ->
      Err Http.NetworkError
    Http.BadStatus_ metadata _ ->
      Err (Http.BadStatus metadata.statusCode)
    Http.GoodStatus_ _ body ->
      Ok body


-- VIEW


viewSong : Theme -> String -> Song -> Html Msg
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
      List.filter Types.File.isImage song.files

    pdfFiles =
      List.filter Types.File.isPdf song.files

    -- Files that are neither pages (images), audio, nor PDFs.
    -- They are offered as plain downloads.
    otherFiles =
      List.filter
        (\file -> not (Types.File.isAudio file)
          && not (Types.File.isImage file)
          && not (Types.File.isPdf file)
        )
        song.files

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
        , if List.isEmpty sheetFiles && List.isEmpty pdfFiles
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
              [ -- Both image pages and PDFs (rendered page by page via
              -- pdf.js) support the horizontal and vertical views
              a
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
        , viewDownloads theme readOnlyId otherFiles
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


viewDownloads : Theme -> String -> List File -> Html Msg
viewDownloads theme readOnlyId otherFiles =
  if List.isEmpty otherFiles
    then text ""
    else
      let
        fileName index file =
          let
            base =
              case file.name of
                Just name ->
                  if String.trim name == ""
                    then "file-" ++ String.fromInt (index + 1)
                    else name
                Nothing ->
                  "file-" ++ String.fromInt (index + 1)
          in
          case file.filetype of
            Just filetype ->
              base ++ "." ++ filetype
            Nothing ->
              base

        viewDownload index file =
          let
            name =
              fileName index file
          in
          li
            [ css [ mb_2 ] ]
            [ button
                [ onClick
                    (DownloadFile
                        name
                        (fileContentUrl readOnlyId file.rowid)
                    )
                , css
                    [ inline_flex
                    , items_center
                    , gap_2
                    , border
                    , rounded
                    , px_2
                    , py_1
                    , bg_color theme.bgRowAlt
                    , cursor_pointer
                    , text_color theme.textPrimary
                    , border_solid
                    , border_color theme.border
                    ]
                ]
                [ span [] [ text "⬇" ]
                , span [] [ text name ]
                ]
            ]
      in
      div
        [ css [ mt_8 ] ]
        [ h3
            [ css [ text_xl, mb_4 ] ]
            [ text "Files" ]
        , ul [] (List.indexedMap viewDownload otherFiles)
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
              { title = "Song not found"
              , body = [ Html.text "Song not found" ]
              }
        Nothing ->
          case ( gqlRes.errors, sharedModel.readonlyId ) of
            ( Just errors, _ ) ->
              { title = "Error"
              , body = [ toUnstyled <| viewGraphQLErrors errors ]
              }
            ( Nothing, Nothing ) ->
              { title = "No Read-Only ID"
              , body = [ Html.text
                    ("No read-only ID is set. "
                      ++ "Open the home page and enter "
                      ++ "your database's read-only ID."
                    )
                ]
              }
            _ ->
              { title = "Loading …"
              , body = [ Html.text "Loading …" ]
              }
    Err error ->
      { title = "Error"
      , body = [ toUnstyled <| viewHttpError error ]
      }
