module Pages.Home_ exposing (Filters, Model, Msg, page)

import Css
import Css.Global
import Effect exposing (Effect(..))
import GraphQL
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import MultiSelect
import Page exposing (Page)
import Route exposing (Route)
import Set
import Shared
import Shared.Model exposing (ColorPref(..))
import Shared.Msg exposing (Msg)
import SmartSelect.Settings as SmartSettings
import Svg.Styled as Svg
import Svg.Styled.Attributes exposing (d, fill, viewBox)
import Tailwind.Breakpoints exposing (..)
import Tailwind.Theme
import Tailwind.Utilities exposing (..)
import Theme exposing (Theme)
import Types.Song exposing (Song)
import Utils exposing (addStarIf, arrowIconVert, host, viewHttpError)
import View exposing (View)


teamplateDbReadonlyId : String
teamplateDbReadonlyId =
  "6mcw4qrv1wvtwn65"


page : Shared.Model -> Route () -> Page Model Msg
page sharedModel _ =
  Page.new
    { init = init sharedModel
    , update = update
    , subscriptions = subscriptions
    , view = view sharedModel
    }


-- INIT
type alias Model =
  { sharedModel : Shared.Model
  , partialReadonlyId : Maybe String
  , searchStrMb : Maybe String
  , filters : Filters
  , instrumentationSelect : MultiSelect.SmartSelect Msg String
  , errors : List String
  }


type alias Filters =
  { interpreter : Maybe String
  , instrumentation : List String
  , key : Maybe String
  , tempo : Maybe String
  }


type FilterField
  = Interpreter
  | Key
  | Tempo


emptyFilters : Filters
emptyFilters =
  { interpreter = Nothing
  , instrumentation = []
  , key = Nothing
  , tempo = Nothing
  }


initInstrumentationSelect : MultiSelect.SmartSelect Msg String
initInstrumentationSelect =
  MultiSelect.init
    { selectionMsg = InstrumentationSelection
    , internalMsg = InstrumentationSelectUpdate
    , idPrefix = "instrumentation-select"
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init sharedModel () =
  ( { sharedModel = sharedModel
    , partialReadonlyId = Nothing
    , searchStrMb = Nothing
    , filters = emptyFilters
    , instrumentationSelect = initInstrumentationSelect
    , errors = []
    }
  , Effect.none
  )


-- UPDATE
type Msg
  = EnteredReadonlyId String
  | EnteredSearch String
  | SubmittedReadonlyId
  | SelectedColorPref ColorPref
  | SelectedFilter FilterField String
  | InstrumentationSelectUpdate (MultiSelect.Msg String)
  | InstrumentationSelection ( List String, MultiSelect.Msg String )
  | ResetFilters


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
  case msg of
    EnteredReadonlyId readonlyId ->
      ( { model | partialReadonlyId = Just readonlyId }
      , Effect.none
      )
    EnteredSearch searchStr ->
      ( { model | searchStrMb = Just searchStr }
      , Effect.none
      )
    SelectedColorPref pref ->
      ( model
      , Effect.sendSharedMsg (Shared.Msg.SetColorPref pref)
      )
    SelectedFilter field val ->
      let
        valMb =
          if val == allFilterMarker
            then Nothing
            else Just val

        f =
          model.filters

        newFilters =
          case field of
            Interpreter ->
              { f | interpreter = valMb }
            Key ->
              { f | key = valMb }
            Tempo ->
              { f | tempo = valMb }
      in
      ( { model | filters = newFilters }
      , Effect.none
      )
    InstrumentationSelectUpdate sMsg ->
      let
        ( newSelect, selectCmd ) =
          MultiSelect.update sMsg model.instrumentationSelect
      in
      ( { model | instrumentationSelect = newSelect }
      , Effect.sendCmd selectCmd
      )
    InstrumentationSelection ( selection, sMsg ) ->
      let
        ( newSelect, selectCmd ) =
          MultiSelect.update sMsg model.instrumentationSelect

        f =
          model.filters

        -- Preserve insertion order: keep previously selected items in their
        -- existing order, then append any newly added ones. The library's
        -- selection list order is not insertion order, so we reconcile here.
        selectionSet =
          Set.fromList selection

        kept =
          f.instrumentation
            |> List.filter (\i -> Set.member i selectionSet)

        added =
          selection
            |> List.filter (\i -> not (List.member i f.instrumentation))

        newInstrumentation =
          kept ++ added
      in
      ( { model
          | instrumentationSelect = newSelect
          , filters = { f | instrumentation = newInstrumentation }
        }
      , Effect.sendCmd selectCmd
      )
    ResetFilters ->
      ( { model | filters = emptyFilters }
      , Effect.none
      )
    SubmittedReadonlyId ->
      let
        lengthNot16 =
          (model.partialReadonlyId |> Maybe.map String.length)
          /= Just 16

        stringNotEmpty =
          (model.partialReadonlyId |> Maybe.map String.isEmpty)
          == Just False
      in
      if lengthNot16 && stringNotEmpty
        then
          ( { model
              | errors = [ """
                        Read-only ID must be 16 characters long.
                        Please make sure you really copied the read-only ID
                        and not the normal database ID.
                        """
                ]
            }
          , Effect.none
          )
        else
          let
            errorRes =
              ( { model
                  | errors = [ "Please enter a read-only ID." ]
                }
              , Effect.none
              )
          in
          case model.partialReadonlyId of
            Nothing ->
              errorRes
            Just "" ->
              errorRes
            Just val ->
              ( { model | partialReadonlyId = Nothing, errors = [] }
              , SendSharedMsg <|
                  Shared.Msg.SubmittedReadonlyId val
              )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  MultiSelect.subscriptions model.instrumentationSelect


-- FILTERS


allFilterMarker : String
allFilterMarker =
  "__filter_all__"


filterBySearchStr : Maybe String -> Song -> Bool
filterBySearchStr searchStrMb song =
  case searchStrMb of
    Just searchStr ->
      ((song.interpreter |> Maybe.withDefault "")
        ++ " "
        ++ song.name
        ++ " "
        ++ (song.instrumentation |> Maybe.withDefault "")
        ++ " "
        ++ (song.key |> Maybe.withDefault "")
      )
        |> String.toLower
        |> String.contains (String.toLower searchStr)
    Nothing ->
      True


filtersActive : Filters -> Bool
filtersActive f =
  f.interpreter
  /= Nothing
  || not (List.isEmpty f.instrumentation)
  || f.key
  /= Nothing
  || f.tempo
  /= Nothing


matchesFilters : Filters -> Song -> Bool
matchesFilters filters song =
  let
    match filterMb val =
      case filterMb of
        Nothing ->
          True
        Just v ->
          val == Just v

    matchInstrumentation =
      let
        songInstrSet =
          songInstrumentations song |> Set.fromList
      in
      filters.instrumentation
        |> List.all (\i -> Set.member i songInstrSet)
  in
  match filters.interpreter song.interpreter
  && matchInstrumentation
  && match filters.key song.key
  && match filters.tempo song.tempo


{-| Split a comma-separated instrumentation string into trimmed,
non-empty parts.
-}
songInstrumentations : Song -> List String
songInstrumentations song =
  song.instrumentation
    |> Maybe.withDefault ""
    |> String.split ","
    |> List.map String.trim
    |> List.filter (not << String.isEmpty)


uniqueValues : (Song -> Maybe String) -> List Song -> List String
uniqueValues getter songs =
  songs
    |> List.filterMap getter
    |> List.filter (not << String.isEmpty << String.trim)
    |> Set.fromList
    |> Set.toList
    |> List.sort


uniqueInstrumentations : List Song -> List String
uniqueInstrumentations songs =
  songs
    |> List.concatMap songInstrumentations
    |> Set.fromList
    |> Set.toList
    |> List.sort


-- VIEW


buttonStyle : Theme -> List Css.Style -> Attribute msg
buttonStyle theme add =
  css <|
    [ inline_block
    , bg_color theme.bgButton
    , text_color theme.textOnAccent
    , rounded
    , w_6
    , h_6
    ]
    ++ add


type SubmitButtonOption
  = HasSubmitButton
  | NoSubmitButton


viewReadonlyIdForm :
  Theme
  -> SubmitButtonOption
  -> Shared.Model
  -> Model
  -> Html Msg
viewReadonlyIdForm theme hasSubmitButton sharedModel model =
  Html.Styled.form
    [ onSubmit SubmittedReadonlyId
    , css [ inline_block ]
    ]
    [ input
        [ type_ "text"
        , placeholder "Read-Only Database ID"
        , value <|
            case model.partialReadonlyId of
              Just val ->
                val
              Nothing ->
                case sharedModel.readonlyId of
                  Just id ->
                    id
                  Nothing ->
                    ""
        , onInput EnteredReadonlyId
        , css
            [ inline_block
            , border
            , border_solid
            , border_color theme.border
            , rounded
            , px_2
            , py_1
            , bg_color theme.bgInput
            , text_color theme.textPrimary
            ]
        ]
        []
    , case hasSubmitButton of
      HasSubmitButton ->
        input
          [ type_ "submit"
          , css
              [ inline_block
              , border
              , border_solid
              , border_color theme.border
              , rounded
              , px_2
              , py_1
              , bg_color theme.bgInput
              , text_color theme.textPrimary
              , ml_2
              , cursor_pointer
              ]
          ]
          [ text "Submit" ]
      NoSubmitButton ->
        text ""
    ]


viewGettingStarted : Theme -> Shared.Model -> Model -> List (Html Msg)
viewGettingStarted theme sharedModel model =
  [ h2 [ css [ text_2xl, mb_8 ] ] [ text "Getting Started" ]
  , ol
      [ css [ list_decimal, ml_8 ] ]
      [ li
          [ css [ mb_6 ] ]
          [ p
              [ css [ mb_4 ] ]
              [ text
                  """
                    Create a new Airsequel database
                    from our sheet music template database:
                    """
              ]
          , a
              [ href <|
                  host
                  ++ "/readonly/"
                  ++ teamplateDbReadonlyId
                  ++ "/duplicate"
              , target "_blank"
              , css
                  [ block
                  , px_6
                  , py_3
                  , rounded
                  , border
                  , border_solid
                  , border_color theme.borderAccent
                  , text_color theme.textOnAccent
                  , max_w_max
                  , bg_color theme.bgAccent
                  ]
              ]
              [ text "Create Database" ]
          ]
      , li
          [ css [ mb_6 ] ]
          [ text
              """
                Add your songs to the database
                via the songs and files tables in the "Tables" tab.
                """
          ]
      , li
          [ css [ mb_6 ] ]
          [ p
              [ css [ mb_4 ] ]
              [ text
                  """
                    Find the read-only ID in the "Database" tab,
                    post it here in the input field, and submit:
                    """
              ]
          , viewReadonlyIdForm theme HasSubmitButton sharedModel model
          ]
      ]
  , hr [ css [ mt_16, mb_4, border_color theme.borderMuted ] ] []
  , footer
      [ css
          [ px_10
          , py_4
          , text_sm
          , text_center
          , text_color theme.textMuted
          ]
      ]
      [ a
          [ href "https://github.com/Airsequel/Airsequel-Sheet-Music"
          , css [ text_color theme.textLink, Css.hover [ underline ] ]
          , target "_blank"
          ]
          [ text "GitHub" ]
      , span [ css [ mx_3 ] ] [ text "•" ]
      , a
          [ href "https://twitter.com/Airsequel"
          , css [ text_color theme.textLink, Css.hover [ underline ] ]
          , target "_blank"
          ]
          [ text "𝕏" ]
      ]
  ]


viewSong : Theme -> Song -> Html msg
viewSong theme song =
  let
    tdSty additions =
      td
        [ css <|
            [ border_x_4
            , border_color theme.bgPanel
            , px_2
            , py_1
            ]
            ++ additions
        ]
  in
  tr
    []
    [ tdSty
        []
        [ text <| Maybe.withDefault "" song.interpreter
        ]
    , tdSty
        []
        [ text <| addStarIf song.isFavorite
        , a
            [ href <| "/songs/" ++ String.fromInt song.rowid
            , css [ underline, text_color theme.textLink ]
            ]
            [ text song.name
            ]
        ]
    , tdSty
        [ px_1 ]
        [ if song.numberOfFiles == 0
          then text ""
          else div
            [ css
                [ flex
                , gap_1
                , justify_center
                ]
            ]
            [ if song.filetypes == Just "pdf"
              then span [ buttonStyle theme [ invisible ] ] [ text "X" ]
              else a
                [ href <|
                    "/songs/horizontal/"
                    ++ String.fromInt song.rowid
                , buttonStyle theme [ p_0_dot_5 ]
                ]
                [ arrowIconVert [ rotate_90 ] ]
            , a
                [ href <|
                    "/songs/vertical/"
                    ++ String.fromInt song.rowid
                , buttonStyle theme [ p_0_dot_5 ]
                ]
                [ arrowIconVert [] ]
            ]
        ]
    , tdSty
        []
        [ text <|
            if song.filetypes == Just "pdf"
              then ""
              else String.fromInt song.numberOfFiles
        ]
    , tdSty [] [ text <| Maybe.withDefault "" song.instrumentation ]
    , tdSty [ text_center ] [ text <| Maybe.withDefault "" song.tempo ]
    , tdSty [ text_center ] [ text <| Maybe.withDefault "" song.key ]
    ]


viewSongsTable : Theme -> Maybe String -> Filters -> List Song -> Html Msg
viewSongsTable theme searchStrMb filters songs =
  let
    documentIcon styles =
      Svg.svg
        [ viewBox "0 0 24 24"
        , fill "currentColor"
        , css styles
        ]
        [ Svg.path
            [ d <|
                "M14 2H6c-1.1 0-2 .9-2 2v16c0 1.1.9 2 2 2"
                ++ "h12c1.1 0 2-.9 2-2V8l-6-6zM6 20V4"
                ++ "h7v5h5v11H6zm10-9h-4v3.88"
                ++ "c-.36-.24-.79-.38-1.25-.38-1.24 0"
                ++ "-2.25 1.01-2.25 2.25"
                ++ "S9.51 19 10.75 19 13 17.99 13 16.75"
                ++ "V13h3v-2z"
            ]
            []
        ]

    thSty additions =
      th
        [ css <|
            [ border_x_4
            , border_color theme.bgPanel
            , px_2
            , py_1
            ]
            ++ additions
        ]

    tableHead =
      thead [] <|
        [ tr
            [ css [ bg_color theme.bgAccentMuted ] ]
            [ thSty [] [ text "Interpreter" ]
            , thSty [] [ text "Song" ]
            , thSty [] [ text "Open" ]
            , thSty
                [ py_0, px_0_dot_5 ]
                [ documentIcon [ inline_block, h_6 ] ]
            , thSty [] [ text "Instrumentation" ]
            , thSty [] [ text "Tempo" ]
            , thSty [] [ text "Key" ]
            ]
        ]

    matchesAll song =
      filterBySearchStr searchStrMb song
      && matchesFilters filters song

    favoriteSongs =
      songs
        |> List.filter matchesAll
        |> List.filter .isFavorite
        |> List.map (viewSong theme)
  in
  Html.Styled.table
    [ css [ w_full, bg_color theme.bgPanel ] ]
    [ tableHead
    , tbody [] <|
        (favoriteSongs
          ++ (if List.isEmpty favoriteSongs
              then []
              else [ tr [ css [ h_8, border_b, border_color theme.border ] ] []
              , tr [ css [ h_8, border_t, border_color theme.border ] ] []
              ]
          )
          ++ (songs
            |> List.filter matchesAll
            |> List.filter (not << .isFavorite)
            |> List.map (viewSong theme)
          )
        )
    ]


formatGqlErrors : Theme -> List GraphQL.Error -> Html msg
formatGqlErrors theme gqlErrors =
  div
    [ css [ text_color theme.textError ] ]
    [ p
        [ css [ font_bold ] ]
        [ text <|
            "Errors: "
            ++ (gqlErrors
              |> List.map
                  (\err -> err.message)
              |> String.join ", "
            )
        ]
    , br [] []
    , p
        []
        [ text
            """
                Please check that the read-only ID is really from a
                copy of the sheet music template database and has
                the correct / up-to-date schema.
                """
        ]
    ]


viewFilterSelect :
  Theme
  -> List Song
  -> String
  -> (Song -> Maybe String)
  -> FilterField
  -> Maybe String
  -> Html Msg
viewFilterSelect theme songs labelText getter field current =
  let
    values =
      uniqueValues getter songs

    optionEl v =
      option
        [ Html.Styled.Attributes.value v ]
        [ text v ]
  in
  Html.Styled.select
    [ onInput (SelectedFilter field)
    , Html.Styled.Attributes.value
        (Maybe.withDefault allFilterMarker current)
    , css
        [ border
        , border_solid
        , border_color theme.border
        , rounded
        , px_2
        , py_1
        , bg_color theme.bgInput -- Gray out the text while the "All …" placeholder is shown,
        -- matching the instrumentation select's placeholder.
        , text_color
            (if current == Nothing
                then theme.textPlaceholder
                else theme.textPrimary
            )
        ]
    ]
    (option
        [ Html.Styled.Attributes.value allFilterMarker ]
        [ text ("All " ++ labelText) ]
        :: List.map optionEl values
    )


{-| Render a selected instrumentation as a pill. The surrounding wrapper
(added by the library) handles click-to-remove, so the whole pill is the
remove affordance; the "×" is just a visual cue.
-}
viewSelectedInstrumentation : String -> Html.Html Msg
viewSelectedInstrumentation instrumentation =
  Html.Styled.toUnstyled <|
    span
      [ css
          [ Css.displayFlex
          , Css.alignItems Css.center
          , Css.property "gap" "0.25rem"
          , Css.padding2 (Css.rem 0.0625) (Css.rem 0.5)
          , Css.backgroundColor (Css.hex "3490dc")
          , Css.color (Css.hex "ffffff")
          , Css.borderRadius (Css.px 9999)
          , Css.fontSize (Css.rem 0.875)
          , Css.lineHeight (Css.rem 1.25)
          , Css.cursor Css.pointer
          ]
      ]
      [ text instrumentation
      , span [ css [ Css.fontSize (Css.rem 1) ] ] [ text "×" ]
      ]


{-| Smart-select settings, themed to match the app's light/dark mode.
The component styles itself via elm-css using this theme record.
-}
instrumentationSelectSettings : Bool -> SmartSettings.Settings Msg
instrumentationSelectSettings darkMode =
  let
    base =
      SmartSettings.defaultSettings

    theme =
      base.theme

    color =
      theme.color

    ( textColor, bgColor, ( borderColor, hoverColor ) ) =
      if darkMode
        then
          ( { primary = Css.hex "f5f5f5"
            , secondary = Css.hex "a3a3a3"
            , disabled = Css.hex "737373"
            }
          , { input = Css.hex "404040"
            , optionsContainer = Css.hex "262626"
            }
          , ( Css.hex "525252", Css.rgba 255 255 255 0.06 )
          )
        else
          ( { primary = Css.hex "1a202c"
            , secondary = Css.hex "4a5568"
            , disabled = Css.hex "a0aec0"
            }
          , { input = Css.hex "ffffff"
            , optionsContainer = Css.hex "ffffff"
            }
          , ( Css.hex "9ca3af", Css.rgba 0 0 0 0.04 )
          )
  in
  { base
    | placeholder = "All Instrumentations"
    , noResultsForMsg = \searchText -> "No results found for: " ++ searchText
    , noOptionsMsg = "No instrumentations available"
    , theme = { theme
        | color = { color
            | text = textColor
            , background = bgColor
            , border = borderColor
            , action = { hover = hoverColor }
          }
      }
  }


viewInstrumentationFilter :
  Bool
  -> List Song
  -> MultiSelect.SmartSelect Msg String
  -> List String
  -> Html Msg
viewInstrumentationFilter darkMode songs select selected =
  div
    [ css [ Css.minWidth (Css.rem 14) ] ]
    [ Html.Styled.fromUnstyled <|
        MultiSelect.view
          { selected = selected
          , options = uniqueInstrumentations songs
          , optionLabelFn = identity
          , viewSelectedOptionFn = viewSelectedInstrumentation
          , settings = instrumentationSelectSettings darkMode
          }
          select
    ]


viewToolbar :
  Theme
  -> Bool
  -> Model
  -> GraphQL.Response (List Song)
  -> List (Html Msg)
viewToolbar theme darkMode model songsResult =
  case songsResult of
    Ok gqlRes ->
      [ case gqlRes.data of
        Just songsData ->
          let
            visibleSongs =
              songsData.root
                |> List.filter (filterBySearchStr model.searchStrMb)
                |> List.filter (matchesFilters model.filters)
          in
          div
            [ css [ flex, flex_col, mt_4, gap_4 ] ]
            [ div
                [ css
                    [ flex
                    , flex_col
                    , sm [ flex_row ]
                    , justify_between
                    , gap_4
                    ]
                ]
                [ div
                    []
                    [ input
                        [ type_ "text"
                        , css
                            [ border
                            , border_solid
                            , border_color theme.border
                            , rounded
                            , px_4
                            , py_2
                            , w_full
                            , bg_color theme.bgInput
                            , text_color theme.textPrimary
                            , sm [ w_80 ]
                            ]
                        , placeholder "Search"
                        , onInput EnteredSearch
                        ]
                        []
                    ]
                , span
                    [ css [ font_semibold ] ]
                    [ text "Number of songs: "
                    , text
                        (visibleSongs
                          |> List.length
                          |> String.fromInt
                        )
                    , if List.length visibleSongs
                      == List.length songsData.root
                      then text ""
                      else text
                        (" / "
                          ++ String.fromInt
                            (List.length songsData.root)
                        )
                    ]
                ]
            , div
                [ css [ flex, flex_wrap, gap_2 ] ]
                [ viewFilterSelect
                    theme
                    songsData.root
                    "Interpreters"
                    .interpreter
                    Interpreter
                    model.filters.interpreter
                , viewInstrumentationFilter
                    darkMode
                    songsData.root
                    model.instrumentationSelect
                    model.filters.instrumentation
                , viewFilterSelect
                    theme
                    songsData.root
                    "Keys"
                    .key
                    Key
                    model.filters.key
                , viewFilterSelect
                    theme
                    songsData.root
                    "Tempos"
                    .tempo
                    Tempo
                    model.filters.tempo
                , if filtersActive model.filters
                  then button
                    [ onClick ResetFilters
                    , css
                        [ bg_color Tailwind.Theme.transparent
                        , border_0
                        , p_0
                        , cursor_pointer
                        , underline
                        , text_color theme.textLink
                        ]
                    ]
                    [ text "Reset filters" ]
                  else text ""
                ]
            ]
        Nothing ->
          text ""
      , case gqlRes.errors of
        Just gqlErrors ->
          formatGqlErrors theme gqlErrors
        Nothing ->
          text ""
      ]
    Err error ->
      [ viewHttpError error ]


colorPrefControl : Theme -> Bool -> ColorPref -> Html Msg
colorPrefControl theme darkMode current =
  let
    nextPref =
      case current of
        Auto ->
          if darkMode
            then Light
            else Dark
        _ ->
          Auto

    label =
      if darkMode
        then "☀"
        else "☾"

    hint =
      case ( current, darkMode ) of
        ( Auto, True ) ->
          "Override to light mode"
        ( Auto, False ) ->
          "Override to dark mode"
        _ ->
          "Reset to system preference"
  in
  button
    [ onClick (SelectedColorPref nextPref)
    , title hint
    , css
        [ cursor_pointer
        , inline_block
        , w_8
        , h_8
        , rounded_full
        , border
        , border_solid
        , border_color theme.border
        , bg_color theme.bgPanel
        , text_color theme.textPrimary
        , text_lg
        , leading_none
        , Css.hover [ bg_color theme.bgRowAlt ]
        ]
    ]
    [ span [ css [ relative ] ] [ text label ] ]


view : Shared.Model -> Model -> View Msg
view sharedModel model =
  let
    darkMode =
      Shared.Model.isDark sharedModel

    theme =
      Theme.fromDarkMode darkMode

    idIsProvided =
      sharedModel.readonlyId
      /= Nothing
      && sharedModel.readonlyId
      /= Just ""

    renderIf boolean html =
      if boolean
        then html
        else text ""
  in
  { title = "Airsequel Sheet Music"
  , body = [ toUnstyled <|
        main_
          [ css
              [ bg_color theme.bgPanel
              , text_color theme.textPrimary
              , max_w_5xl
              , mx_auto
              , min_h_full
              , border_color theme.border
              , lg [ border_x ]
              ]
          ]
          [ Css.Global.global globalStyles
          , Css.Global.global (Theme.globalSnippets darkMode)
          , nav
              [ css
                  [ p_4
                  , sm [ px_10, pt_12, pb_4 ]
                  ]
              ]
              (div
                  [ css
                      [ flex
                      , flex_col
                      , sm [ flex_row ]
                      , items_center
                      , gap_3
                      , pb_4
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
                      [ text "Airsequel Sheet Music" ]
                  , renderIf idIsProvided <|
                      div
                        [ css [ pt_1_dot_5, text_sm ] ]
                        [ label
                            [ css [ text_color theme.textMuted ] ]
                            [ text "ID: " ]
                        , viewReadonlyIdForm
                            theme
                            NoSubmitButton
                            sharedModel
                            model
                        ]
                  , colorPrefControl theme darkMode sharedModel.colorPref
                  ]
                  :: viewToolbar theme darkMode model sharedModel.songsResult
              )
          , div
              [ css [ overflow_scroll, pb_12, sm [ px_10 ] ] ]
              ((renderIf (not idIsProvided)
                  <|
                    div
                      []
                      [ p
                          [ css [ mb_8 ] ]
                          [ text "Sheet music management app powered by "
                          , a
                              [ href "https://www.airsequel.com"
                              , css [ underline, text_color theme.textLink ]
                              , target "_blank"
                              ]
                              [ text "Airsequel" ]
                          , text "."
                          ]
                      , img
                          [ css [ max_w_lg, shadow_md, mb_8 ]
                          , src "/landing_page.png"
                          , alt "Screenshot of landing page"
                          ]
                          []
                      ]
                )
                  :: (let
                          ifNothing valMb valDefault =
                            case valMb of
                              Nothing ->
                                valDefault
                              _ ->
                                []

                          errorPara errorTxt =
                            p
                              [ css
                                  [ bg_color theme.bgError
                                  , border
                                  , border_solid
                                  , border_color theme.borderError
                                  , text_color theme.textError
                                  , rounded
                                  , px_4
                                  , py_2
                                  , mb_4
                                  , max_w_xl
                                  ]
                              ]
                              [ text errorTxt ]
                        in
                        (case sharedModel.songsResult of
                            Ok gqlRes ->
                              case gqlRes.data of
                                Just songsData ->
                                  [ viewSongsTable
                                      theme
                                      model.searchStrMb
                                      model.filters
                                      songsData.root
                                  ]
                                Nothing ->
                                  let
                                    readonlyIdEmpty =
                                      (sharedModel.readonlyId
                                        == Nothing
                                      )
                                      || (sharedModel.readonlyId
                                        == Just ""
                                      )
                                  in
                                  if readonlyIdEmpty
                                    then viewGettingStarted
                                      theme
                                      sharedModel
                                      model
                                    else ifNothing
                                      gqlRes.errors
                                      [ div
                                          [ css [ text_center ] ]
                                          [ text "Loading …" ]
                                      ]
                            Err httpError ->
                              [ viewHttpError httpError ]
                        )
                        ++ [ div
                            []
                            (model.errors |> List.map errorPara)
                        ]
                    )
              )
          ]
    ]
  }
