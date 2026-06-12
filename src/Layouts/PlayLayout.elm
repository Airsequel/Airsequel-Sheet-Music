module Layouts.PlayLayout exposing
  ( Alignment(..)
  , Model
  , Msg
  , Props
  , layout
  )

import Css
import Dict
import Effect exposing (Effect)
import GraphQL
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Layout exposing (Layout)
import Maybe exposing (withDefault)
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Theme exposing (..)
import Tailwind.Utilities exposing (..)
import Theme
import Types.File exposing (File)
import Types.ReadDirection exposing (ReadDirection(..))
import Types.Song exposing (Song)
import Types.SongSettings exposing (ColorScheme(..), SongSettings)
import Utils exposing (fileContentUrl, viewGraphQLErrors, viewHttpError)
import View exposing (View)


type alias Props =
  { songId : String
  , readDirection : ReadDirection
  , songsResult : GraphQL.Response (List Song)
  }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout settings sharedModel _ =
  Layout.new
    { init = init settings sharedModel
    , update = update settings
    , view = view settings sharedModel
    , subscriptions = subscriptions
    }


htmlIf : Bool -> Html msg -> Html msg
htmlIf check htmlVal =
  if check
    then htmlVal
    else text ""


-- MODEL
type Alignment
  = AlignTop
  | AlignCenter
  | AlignBottom


{-| Solarized colors <https://ethanschoonover.com/solarized/>
-}
sepiaColors : { fg : Css.Color, bg : Css.Color }
sepiaColors =
  { fg = Css.rgb 7 54 66
  , bg = Css.rgb 253 246 227
  }


type alias Model =
  { -- alignment : Alignment, -- TODO: Is there still a need for this?
  colorScheme : ColorScheme
  , showHeading : Bool
  , showPageNumbers : Maybe Bool-- Nothing = hide for songs with up to 2 pages
  , playingAudio : Maybe Int
  , pageMaxWidth : Float-- In rem
  , centerPages : Bool
  , showDividers : Bool
  }


pageMaxWidthDefault : Float
pageMaxWidthDefault =
  72


pageMaxWidthStep : Float
pageMaxWidthStep =
  8


defaultSettings : Shared.Model -> ReadDirection -> SongSettings
defaultSettings sharedModel readDirection =
  { colorScheme =
      if Shared.Model.isDark sharedModel
        then Dark
        else Light
  , showHeading = case readDirection of
      ReadHorizontal ->
        False
      ReadVertical ->
        True
  , showPageNumbers = case readDirection of
      ReadHorizontal ->
        Nothing
      ReadVertical ->
        Just True
  , pageMaxWidth = pageMaxWidthDefault
  , centerPages = True
  , showDividers = True
  }


{-| Without an explicit user choice, page numbers are only shown
for songs with more than 2 pages
-}
resolveShowPageNumbers : Model -> Int -> Bool
resolveShowPageNumbers model numOfPages =
  model.showPageNumbers
    |> Maybe.withDefault (numOfPages > 2)


toSongSettings : Model -> SongSettings
toSongSettings model =
  { colorScheme = model.colorScheme
  , showHeading = model.showHeading
  , showPageNumbers = model.showPageNumbers
  , pageMaxWidth = model.pageMaxWidth
  , centerPages = model.centerPages
  , showDividers = model.showDividers
  }


init : Props -> Shared.Model -> () -> ( Model, Effect Msg )
init props sharedModel _ =
  let
    -- Stored settings only apply to the horizontal view;
    -- the vertical view may get its own styling options later
    settings =
      case props.readDirection of
        ReadHorizontal ->
          Dict.get props.songId sharedModel.horizontalSongSettings
            |> Maybe.withDefault
                (defaultSettings sharedModel props.readDirection)
        ReadVertical ->
          defaultSettings sharedModel props.readDirection
  in
  ( { -- alignment = AlignTop,
    colorScheme = settings.colorScheme
    , showHeading = settings.showHeading
    , showPageNumbers = settings.showPageNumbers
    , playingAudio = Nothing
    , pageMaxWidth = settings.pageMaxWidth
    , centerPages = settings.centerPages
    , showDividers = settings.showDividers
    }
  , Effect.none
  )


-- UPDATE
type Msg
  = -- SetAlignment Alignment |
  SetColorScheme ColorScheme
  | SetShowHeading Bool
  | SetShowPageNumbers Bool
  | AdjustPageMaxWidth Float
  | SetCenterPages Bool
  | SetShowDividers Bool
  | ToggleAudio Int


update : Props -> Msg -> Model -> ( Model, Effect Msg )
update props msg model =
  let
    -- Apply a settings change and store it for this song
    -- in local storage (only in the horizontal view)
    persist : Model -> ( Model, Effect Msg )
    persist newModel =
      ( newModel
      , case props.readDirection of
          ReadHorizontal ->
            Effect.sendSharedMsg
              (Shared.Msg.SetHorizontalSongSettings
                  props.songId
                  (toSongSettings newModel)
              )
          ReadVertical ->
            Effect.none
      )
  in
  case msg of
    -- SetAlignment alignment ->
    --     ( { model | alignment = alignment }
    --     , Effect.none
    --     )
    SetColorScheme colorScheme ->
      persist { model | colorScheme = colorScheme }
    SetShowHeading val ->
      persist { model | showHeading = val }
    SetShowPageNumbers val ->
      persist { model | showPageNumbers = Just val }
    AdjustPageMaxWidth delta ->
      persist
        { model
          | pageMaxWidth = clamp 24 200 (model.pageMaxWidth + delta)
        }
    SetCenterPages val ->
      persist { model | centerPages = val }
    SetShowDividers val ->
      persist { model | showDividers = val }
    ToggleAudio rowid ->
      ( { model
          | playingAudio =
              if model.playingAudio == Just rowid
                then Nothing
                else Just rowid
        }
      , Effect.none
      )


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- VIEW


viewImage : Song -> ReadDirection -> Model -> String -> Int -> File -> Html msg
viewImage song readDirection model readOnlyId index file =
  let
    pageNumHeight =
      Css.rem 2

    headerHeight =
      Css.rem 5

    numOfPages =
      song.files
        |> List.filter (Types.File.isAudio >> not)
        |> List.length

    showPageNumbers =
      resolveShowPageNumbers model numOfPages
  in
  div
    [ id "viewImage"
    , css
        [ flex
        , flex_col
        , border_solid
        , border_color orange_500
        , p_2
        , case model.colorScheme of
          Light ->
            Css.property "" ""
          Dark ->
            Css.batch
              [ Css.property "filter" "invert(1)"
              , Css.opacity (Css.num 0.85)
              ]
          Sepia ->
            Css.property "mix-blend-mode" "multiply"
        , if model.showDividers
          then case readDirection of
            ReadHorizontal ->
              if index < numOfPages - 1
                then border_r_2
                else Css.batch []
            ReadVertical ->
              border_b_2
          else Css.batch []
        ]
    ]
    [ htmlIf showPageNumbers <|
        div
          [ css [ Css.height pageNumHeight, text_center ] ]
          [ p
              [ css [ font_sans, text_sm ] ]
              [ text <|
                  String.fromInt (index + 1)
                  ++ " / "
                  ++ String.fromInt numOfPages
              ]
          ]
    , htmlIf (model.showHeading && index == 0)
    <|
        div
          [ css [ Css.height headerHeight, text_center ] ]
          (if model.showHeading && index == 0
              then [ h2
                  [ css [ font_sans, font_medium, mb_2 ] ]
                  [ text song.name ]
              , p [] [ song.interpreter |> withDefault "" |> text ]
              ]
              else []
          )
    , div
        [ css <|
            [ flex
            , flex_col -- , case model.alignment of
            --     AlignTop ->
            --         justify_start
            --     AlignCenter ->
            --         justify_center
            --     AlignBottom ->
            --         justify_end
            ]
            ++ (if (model.showHeading && index == 0) || showPageNumbers
                then [ Css.height <|
                    Css.calc
                      (Css.pct 100)
                      Css.minus
                      (Css.calc
                          (if showPageNumbers
                              then pageNumHeight
                              else Css.rem 0
                          )
                          Css.plus
                          (if model.showHeading && index == 0
                              then headerHeight
                              else Css.rem 0
                          )
                      )
                , overflow_hidden
                ]
                else [ h_full ]
            )
        ]
        [ img
            [ src (fileContentUrl readOnlyId file.rowid)
            , css <|
                case readDirection of
                  ReadHorizontal ->
                    [ block -- Prevent wide images from expanding too much
                    , Css.maxWidth (Css.rem model.pageMaxWidth) -- Prevent image from losing aspect ratio
                    , object_contain
                    , if numOfPages > 1
                      then h_full
                      else Css.batch
                        [ max_w_full
                        , max_h_full
                        ]
                    ]
                  ReadVertical ->
                    [ block, w_full, max_w_6xl, self_center ]
            ]
            []
        ]
    ]


filesAreType : String -> List File -> Bool
filesAreType filetype files =
  files
    |> List.all
        (\file -> (file.filetype |> Maybe.map String.toLower)
          == Just (String.toLower filetype)
        )


getSidebar : Shared.Model -> Model -> String -> String -> Int -> List File -> Html Msg
getSidebar sharedModel model readOnlyId songId numOfPages audioFiles =
  let
    theme =
      Theme.fromDarkMode (Shared.Model.isDark sharedModel)

    colorScheme =
      model.colorScheme

    btnCss =
      Css.batch
        [ cursor_pointer
        , py_1
        , bg_color theme.sidebarBtn
        , Css.hover [ bg_color theme.sidebarBtnHover ]
        , Css.active [ bg_color theme.sidebarBtnActive ]
        , text_center
        ]

    markSelectedFor : a -> a -> Css.Style
    markSelectedFor reference actual =
      if reference == actual
        then Css.batch
          [ border_l_4
          , border_l_color orange_500
          , border_solid
          , bg_color theme.sidebarBtnSelected
          ]
        else Css.batch
          [ border_l_4
          , border_solid
          , border_color transparent
          ]

    backButton =
      [ a
          [ css
              [ btnCss
              , markSelectedFor True False
              , no_underline
              , Css.color Css.inherit
              , font_sans
              ]
          , title "Back to song details"
          , href ("/songs/" ++ songId)
          ]
          [ text "←" ]
      ]

    formattingButtons =
      [ button
          [ css [ btnCss, markSelectedFor True model.showHeading ]
          , onClick (SetShowHeading (not model.showHeading))
          ]
          [ text "H" ]
      , button
          [ css
              [ btnCss
              , markSelectedFor
                  True
                  (resolveShowPageNumbers model numOfPages)
              ]
          , title "Show page numbers"
          , onClick
              (SetShowPageNumbers
                  (not (resolveShowPageNumbers model numOfPages))
              )
          ]
          [ span
              [ css [ font_sans, text_xs ] ]
              [ text ("1/" ++ String.fromInt numOfPages) ]
          ]
      , button
          [ css [ btnCss, markSelectedFor True model.centerPages ]
          , title "Center pages horizontally"
          , onClick (SetCenterPages (not model.centerPages))
          ]
          [ text "↔" ]
      , button
          [ css [ btnCss, markSelectedFor True model.showDividers ]
          , title "Show divider lines between pages"
          , onClick (SetShowDividers (not model.showDividers))
          ]
          [ span
              [ css [ text_color orange_500 ] ]
              [ text "|" ]
          ]
      ]

    -- alignmentButtons =
    --     [ button
    --         [ css [ btnCss, markSelectedFor AlignTop model.alignment ]
    --         , title "Align pages at the top"
    --         , onClick (SetAlignment AlignTop)
    --         ]
    --         [ text "⬆️" ]
    --     , button
    --         [ css [ btnCss, markSelectedFor AlignCenter model.alignment ]
    --         , title "Center pages"
    --         , onClick (SetAlignment AlignCenter)
    --         ]
    --         [ text "⏺" ]
    --     , button
    --         [ css [ btnCss, markSelectedFor AlignBottom model.alignment ]
    --         , title "Align pages at the bottom"
    --         , onClick (SetAlignment AlignBottom)
    --         ]
    --         [ text "⬇️" ]
    --     ]
    iconStyle =
      Css.batch
        [ inline_block
        , w_5
        , h_5
        , rounded_full
        , border
        , border_solid
        , border_color gray_400
        , bg_color gray_100
        , text_center
        ]

    iconContent =
      [ span
          [ css [ relative, top_0_dot_5 ] ]
          [ text "♫" ]
      ]

    colorSchemeButtons =
      [ button
          [ css [ btnCss, markSelectedFor Light colorScheme ]
          , title "Light color scheme"
          , onClick (SetColorScheme Light)
          ]
          [ span
              [ css [ iconStyle, bg_color white, text_color black ] ]
              iconContent
          ]
      , button
          [ css [ btnCss, markSelectedFor Dark colorScheme ]
          , title "Dark color scheme"
          , onClick (SetColorScheme Dark)
          ]
          [ span
              [ css [ iconStyle, bg_color black, text_color white ]
              ]
              iconContent
          ]
      , button
          [ css [ btnCss, markSelectedFor Sepia colorScheme ]
          , title "Sepia color scheme"
          , onClick (SetColorScheme Sepia)
          ]
          [ span
              [ css
                  [ iconStyle
                  , Css.backgroundColor sepiaColors.bg
                  , Css.color sepiaColors.fg
                  ]
              ]
              iconContent
          ]
      ]

    pageWidthButtons =
      [ button
          [ css [ btnCss, markSelectedFor True False ]
          , title "Increase max width of pages"
          , onClick (AdjustPageMaxWidth pageMaxWidthStep)
          ]
          [ text "+" ]
      , button
          [ css [ btnCss, markSelectedFor True False ]
          , title "Decrease max width of pages"
          , onClick (AdjustPageMaxWidth -pageMaxWidthStep)
          ]
          [ text "−" ]
      ]

    placeholder =
      [ div [ css [ h_3 ] ] [] ]

    audioButton index file =
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
      button
        [ css [ btnCss, markSelectedFor (Just file.rowid) model.playingAudio ]
        , title label
        , onClick (ToggleAudio file.rowid)
        ]
        [ text
            (if model.playingAudio == Just file.rowid
                then "⏹️"
                else "▶️"
            )
        ]

    -- Hidden element that actually plays the selected track. Keyed on the
    -- rowid so switching tracks remounts it (restarting playback); clearing
    -- playingAudio unmounts it and stops playback.
    audioPlayer rowid =
      Keyed.node
        "div"
        [ css [ Css.display Css.none ] ]
        [ ( String.fromInt rowid
        , audio
            [ src (fileContentUrl readOnlyId rowid)
            , autoplay True
            ]
            []
        )
        ]

    audioControls =
      case audioFiles of
        [] ->
          []
        _ ->
          placeholder
          ++ List.indexedMap audioButton audioFiles
          ++ [ model.playingAudio
            |> Maybe.map audioPlayer
            |> Maybe.withDefault (text "")
          ]
  in
  div
    [ css
        [ flex
        , flex_col
        , flex_shrink_0
        , w_12
        , h_full
        , align_top
        , gap_y_0_dot_5
        , bg_color theme.sidebarBg
        ]
    ]
    (backButton
      ++ placeholder
      ++ formattingButtons
      ++ placeholder-- ++ alignmentButtons
      -- ++ placeholder
      ++ colorSchemeButtons
      ++ placeholder
      ++ pageWidthButtons
      ++ audioControls
    )


viewSong : Shared.Model -> ReadDirection -> Model -> String -> Song -> Html Msg
viewSong sharedModel readDirection model readOnlyId song =
  let
    sheetFiles : List File
    sheetFiles =
      List.filter (Types.File.isAudio >> not) song.files

    divImages : List (Html Msg) -> Html Msg
    divImages content =
      div
        [ id "divImages"
        , css
            [ whitespace_nowrap
            , flex
            , case readDirection of
              ReadHorizontal ->
                Css.batch
                  [ flex_row -- , case model.alignment of
                  --     AlignTop ->
                  --         self_start
                  --     AlignCenter ->
                  --         self_center
                  --     AlignBottom ->
                  --         self_end
                  ]
              ReadVertical ->
                flex_col
            , h_full
            ]
        ]
        content

    divCenter : List (Html msg) -> Html msg
    divCenter content =
      div
        [ css [ text_center, font_sans, pt_8 ] ]
        content
  in
  if List.isEmpty sheetFiles
    then divCenter [ text "No files" ]
    else
      if sheetFiles |> filesAreType "pdf"
        then case sheetFiles of
          [file] ->
            divImages <|
              [ iframe
                  [ src (fileContentUrl readOnlyId file.rowid)
                  , css [ w_full, h_full, border_none ]
                  ]
                  []
              ]
          _ :: _ ->
            divCenter [ text "Does not support more than one PDF per song" ]
          _ ->
            divCenter [ text "No files" ]
        else
          let
            pageImages =
              sheetFiles
                |> List.indexedMap
                    (viewImage
                        song
                        readDirection
                        model
                        readOnlyId
                    )
          in
          divImages <|
            case readDirection of
              ReadHorizontal ->
                [ getSidebar
                    sharedModel
                    model
                    readOnlyId
                    (String.fromInt song.rowid)
                    (List.length sheetFiles)
                    (List.filter Types.File.isAudio song.files)
                , div
                    [ css <|
                        [ flex, flex_row, h_full ]-- Auto margins center the pages when they are
                        -- narrower than the viewport and collapse to 0
                        -- when they overflow, so scrolling still works
                        ++ (if model.centerPages
                            then [ mx_auto ]
                            else []
                        )
                    ]
                    pageImages
                ]
              ReadVertical ->
                pageImages


viewPages : Props -> Shared.Model -> Model -> Song -> Html Msg
viewPages settings sharedModel model song =
  let
    readOnlyId =
      sharedModel.readonlyId
        |> Maybe.withDefault ""
  in
  div
    (css
        [ case settings.readDirection of
          ReadHorizontal ->
            h_full
          ReadVertical ->
            if filesAreType "pdf" song.files
              then h_full
              else w_full
        , case model.colorScheme of
          Light ->
            bg_color white
          Dark ->
            bg_color black
          Sepia ->
            Css.backgroundColor sepiaColors.bg -- Make bg color cover the whole page:
        , overflow_scroll
        ]
        :: (case settings.readDirection of
            ReadHorizontal ->
              [ attribute "data-scroll-direction" "horizontal" ]
            ReadVertical ->
              []
        )
    )
    [ viewSong sharedModel settings.readDirection model readOnlyId song ]


view :
  Props
  -> Shared.Model
  -> { toContentMsg : Msg -> mainMsg
  , content : View mainMsg
  , model : Model
  }
  -> View mainMsg
view settings sharedModel { toContentMsg, model } =
  case settings.songsResult of
    Ok gqlRes ->
      case gqlRes.data of
        Just songs ->
          case songs.root of
            song :: _ ->
              { title = song.name ++ " | Play View"
              , body = [ toUnstyled <|
                    (viewPages settings sharedModel model song
                      |> Html.Styled.map toContentMsg
                    )
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
              , body = [ toUnstyled <|
                    div
                      [ css [ text_center, font_sans, pt_8 ] ]
                      [ text <|
                          "No read-only ID is set. "
                          ++ "Open the home page and enter "
                          ++ "your database's read-only ID."
                      ]
                ]
              }
            _ ->
              { title = "Loading …"
              , body = [ toUnstyled <|
                    div
                      [ css [ text_center, font_sans, pt_8 ] ]
                      [ text "Loading …" ]
                ]
              }
    Err httpError ->
      { title = "Error"
      , body = [ toUnstyled <| viewHttpError httpError ]
      }
