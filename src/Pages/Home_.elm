module Pages.Home_ exposing (Model, Msg, page)

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
import Shared.Model
  exposing
    ( ColorPref(..)
    , Filters
    , emptyFilters
    , filtersActive
    )
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
  , filters : Filters
  , instrumentationSelect : MultiSelect.SmartSelect Msg String
  , errors : List String
  }


type FilterField
  = Interpreter
  | Key
  | Tempo


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
  | SelectedPage Int


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
  case msg of
    EnteredReadonlyId readonlyId ->
      ( { model | partialReadonlyId = Just readonlyId }
      , Effect.none
      )
    EnteredSearch searchStr ->
      ( model
      , Effect.sendSharedMsg (Shared.Msg.EnteredSongsSearch searchStr)
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
      , Effect.sendSharedMsg (Shared.Msg.SetSongsFilters newFilters)
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

        newFilters =
          { f | instrumentation = newInstrumentation }
      in
      ( { model
          | instrumentationSelect = newSelect
          , filters = newFilters
        }
      , Effect.batch
          [ Effect.sendCmd selectCmd
          , Effect.sendSharedMsg (Shared.Msg.SetSongsFilters newFilters)
          ]
      )
    ResetFilters ->
      ( { model | filters = emptyFilters }
      , Effect.sendSharedMsg Shared.Msg.ResetSongsSearchAndFilters
      )
    SelectedPage pageNumber ->
      ( model
      , Effect.sendSharedMsg (Shared.Msg.SelectedSongsPage pageNumber)
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


viewSongsTable : Theme -> Bool -> List Song -> Html Msg
viewSongsTable theme isLoading songs =
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

    bodyRows =
      if isLoading
        then [ tr
            []
            [ td
                [ colspan 7
                , css [ text_center, py_8, text_color theme.textMuted ]
                ]
                [ text "Loading …" ]
            ]
        ]
        else
          let
            favoriteSongs =
              songs
                |> List.filter .isFavorite
                |> List.map (viewSong theme)

            otherSongs =
              songs
                |> List.filter (not << .isFavorite)
                |> List.map (viewSong theme)

            separator =
              if List.isEmpty favoriteSongs || List.isEmpty otherSongs
                then []
                else [ tr [ css [ h_8, border_b, border_color theme.border ] ] []
                , tr [ css [ h_8, border_t, border_color theme.border ] ] []
                ]
          in
          favoriteSongs ++ separator ++ otherSongs
  in
  Html.Styled.table
    [ css [ w_full, bg_color theme.bgPanel ] ]
    [ tableHead
    , tbody [] bodyRows
    ]


{-| Page controls for the server-side pagination.
Without a search or filters, the total number of pages is derived from
the `total_count` column of the `songs_paginated_json` view and
numbered page buttons are shown. While searching or filtering,
`total_count` is meaningless (the window function ignores the filter),
so only prev/next controls are shown, based on whether an extra row
could be fetched.
-}
viewPagination : Theme -> Shared.Model -> Int -> Html Msg
viewPagination theme sharedModel totalCount =
  let
    currentPage =
      sharedModel.songsPage

    -- Shared geometry so buttons and ellipsis line up as equal boxes.
    boxStyles =
      [ Css.minWidth (Css.rem 2.5)
      , h_10
      , px_2
      , inline_flex
      , items_center
      , justify_center
      , rounded_lg
      , text_lg
      ]

    pageButton isActive isDisabled msg label =
      button
        [ onClick msg
        , Html.Styled.Attributes.disabled isDisabled
        , css
            (boxStyles
              ++ [ border
              , border_solid
              , border_color theme.borderMuted
              , text_color
                  (if isActive
                      then theme.textOnAccent
                      else theme.textPrimary
                  )
              , bg_color
                  (if isActive
                      then theme.bgAccent
                      else theme.bgInput
                  )
              , if isActive
                then font_semibold
                else font_medium
              , if isDisabled
                then Css.batch [ opacity_50, cursor_not_allowed ]
                else cursor_pointer
              ]
            )
        ]
        [ text label ]
  in
  if sharedModel.songsSearch
    /= Nothing
    || filtersActive sharedModel.songsFilters
    then if currentPage == 1 && not sharedModel.hasNextSongsPage
      then text ""
      else div
        [ css [ flex, items_center, justify_center, gap_2, mt_6 ] ]
        [ pageButton
            False
            (currentPage == 1)
            (SelectedPage (currentPage - 1))
            "‹"
        , span
            [ css [ font_semibold, px_2 ] ]
            [ text ("Page " ++ String.fromInt currentPage) ]
        , pageButton
            False
            (not sharedModel.hasNextSongsPage)
            (SelectedPage (currentPage + 1))
            "›"
        ]
    else
      let
        numberOfPages =
          Basics.max 1 <|
            (totalCount + Shared.Model.songsPerPage - 1)
            // Shared.Model.songsPerPage
      in
      if numberOfPages <= 1
        then text ""
        else
          let
            firstResult =
              (currentPage - 1) * Shared.Model.songsPerPage + 1

            lastResult =
              Basics.min (currentPage * Shared.Model.songsPerPage) totalCount

            ellipsis =
              span
                [ css (boxStyles ++ [ text_color theme.textMuted ]) ]
                [ text "…" ]
          in
          div
            [ css [ flex, flex_col, items_center, gap_3, mt_6 ] ]
            [ div
                [ css [ flex, flex_wrap, items_center, justify_center, gap_2 ] ]
                (pageButton
                    False
                    (currentPage == 1)
                    (SelectedPage (currentPage - 1))
                    "‹"
                    :: (paginationWindow currentPage numberOfPages
                      |> List.map
                          (\item -> case item of
                              Just pageNumber ->
                                pageButton
                                  (pageNumber == currentPage)
                                  False
                                  (SelectedPage pageNumber)
                                  (String.fromInt pageNumber)
                              Nothing ->
                                ellipsis
                          )
                    )
                  ++ [ pageButton
                      False
                      (currentPage == numberOfPages)
                      (SelectedPage (currentPage + 1))
                      "›"
                  ]
                )
            , span
                [ css [ font_semibold, text_color theme.textSecondary ] ]
                [ text
                    ("Results: "
                      ++ String.fromInt firstResult
                      ++ " - "
                      ++ String.fromInt lastResult
                      ++ " of "
                      ++ String.fromInt totalCount
                    )
                ]
            ]


{-| Page numbers to display, with `Nothing` marking an ellipsis gap.
Always keeps the first and last page plus a small window around the
current page, e.g. `1 … 4 5 6 … 10`.
-}
paginationWindow : Int -> Int -> List (Maybe Int)
paginationWindow current total =
  let
    windowSize =
      3

    start =
      clamp 1 (Basics.max 1 (total - windowSize + 1)) (current - 1)

    end =
      Basics.min (start + windowSize - 1) total

    numbers =
      (1 :: List.range start end ++ [ total ])
        |> dedupeSorted
  in
  numbers
    |> List.foldr
        (\n acc -> case acc of
            (Just next) :: _ ->
              if next - n > 1
                then Just n :: Nothing :: acc
                else Just n :: acc
            _ ->
              Just n :: acc
        )
        []


{-| Drop consecutive duplicates from an ascending list.
-}
dedupeSorted : List Int -> List Int
dedupeSorted =
  List.foldr
    (\x acc -> case acc of
        y :: _ ->
          if x == y
            then acc
            else x :: acc
        [] ->
          [ x ]
    )
    []


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
  -> List String
  -> String
  -> FilterField
  -> Maybe String
  -> Html Msg
viewFilterSelect theme values labelText field current =
  let
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
  -> List String
  -> MultiSelect.SmartSelect Msg String
  -> List String
  -> Html Msg
viewInstrumentationFilter darkMode options select selected =
  div
    [ css [ Css.minWidth (Css.rem 14) ] ]
    [ Html.Styled.fromUnstyled <|
        MultiSelect.view
          { selected = selected
          , options = options
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
  -> Shared.Model
  -> List (Html Msg)
viewToolbar theme darkMode model sharedModel =
  case sharedModel.songsResult of
    Ok gqlRes ->
      [ case gqlRes.data of
        Just songsData ->
          let
            totalIsUnknown =
              sharedModel.songsSearch
              /= Nothing
              || filtersActive sharedModel.songsFilters

            visibleSongs =
              songsData.root.songs

            -- Distinct values over the whole collection; databases
            -- without the `filter_options_json` view fall back
            -- to the values of the currently loaded page
            filterOptions =
              case sharedModel.filterOptions of
                Just options ->
                  options
                Nothing ->
                  { interpreters = uniqueValues .interpreter songsData.root.songs
                  , instrumentations = uniqueInstrumentations songsData.root.songs
                  , keys = uniqueValues .key songsData.root.songs
                  , tempos = uniqueValues .tempo songsData.root.songs
                  }
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
                        , value
                            (sharedModel.songsSearch
                              |> Maybe.withDefault ""
                            )
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
                    , -- While searching or filtering, total_count still
                    -- refers to the unfiltered collection,
                    -- so it is not shown
                    if totalIsUnknown
                      || List.length visibleSongs
                      == songsData.root.totalCount
                      then text ""
                      else text
                        (" / "
                          ++ String.fromInt
                            songsData.root.totalCount
                        )
                    ]
                ]
            , div
                [ css [ flex, flex_wrap, gap_2 ] ]
                [ viewFilterSelect
                    theme
                    filterOptions.interpreters
                    "Interpreters"
                    Interpreter
                    model.filters.interpreter
                , viewInstrumentationFilter
                    darkMode
                    filterOptions.instrumentations
                    model.instrumentationSelect
                    model.filters.instrumentation
                , viewFilterSelect
                    theme
                    filterOptions.keys
                    "Keys"
                    Key
                    model.filters.key
                , viewFilterSelect
                    theme
                    filterOptions.tempos
                    "Tempos"
                    Tempo
                    model.filters.tempo
                , if filtersActive model.filters
                  || sharedModel.songsSearch
                  /= Nothing
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
                    [ text "Reset search & filters" ]
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
                  :: viewToolbar theme darkMode model sharedModel
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
                                      sharedModel.songsLoading
                                      songsData.root.songs
                                  , viewPagination
                                      theme
                                      sharedModel
                                      songsData.root.totalCount
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
