module SongsTable exposing (tableHead, viewSong, viewTable)

{-| Shared rendering of the songs table (header + one row per song),
used by both the home page and the playlist detail page.

@docs tableHead, viewSong, viewTable

-}

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Svg.Styled as Svg
import Svg.Styled.Attributes exposing (d, fill, viewBox)
import Tailwind.Utilities exposing (..)
import Theme exposing (Theme)
import Types.Song exposing (Song)
import Utils exposing (addStarIf, arrowIconVert)


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


documentIcon : List Css.Style -> Html msg
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


tableHead : Theme -> Html msg
tableHead theme =
  let
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
  in
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
            [ a
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


{-| A complete songs table (header + rows) in the given order.
-}
viewTable : Theme -> List Song -> Html msg
viewTable theme songs =
  Html.Styled.table
    [ css [ w_full, bg_color theme.bgPanel ] ]
    [ tableHead theme
    , tbody [] (List.map (viewSong theme) songs)
    ]
