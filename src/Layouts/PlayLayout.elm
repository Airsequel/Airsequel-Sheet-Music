module Layouts.PlayLayout exposing
    ( Alignment(..)
    , ColorScheme(..)
    , Model
    , Msg
    , Props
    , layout
    )

import Css
import Effect exposing (Effect)
import GraphQL
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Layout exposing (Layout)
import Maybe exposing (withDefault)
import Route exposing (Route)
import Shared
import Tailwind.Theme exposing (..)
import Tailwind.Utilities exposing (..)
import Types.File exposing (File)
import Types.ReadDirection exposing (ReadDirection(..))
import Types.Song exposing (Song)
import Utils exposing (host, viewHttpError)
import View exposing (View)


type alias Props =
    { songId : String
    , readDirection : ReadDirection
    , songsResult : GraphQL.Response (List Song)
    }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout settings sharedModel _ =
    Layout.new
        { init = init
        , update = update
        , view = view settings sharedModel
        , subscriptions = subscriptions
        }



-- MODEL


type Formatting
    = DefaultFormatting
    | ShowHeading


type Alignment
    = AlignTop
    | AlignCenter
    | AlignBottom


type ColorScheme
    = Light
    | Dark
    | Sepia


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
    , formatting : Formatting
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { -- alignment = AlignTop,
        colorScheme = Light
      , formatting = ShowHeading
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = -- SetAlignment Alignment |
      SetColorScheme ColorScheme
    | SetFormatting Formatting


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        -- SetAlignment alignment ->
        --     ( { model | alignment = alignment }
        --     , Effect.none
        --     )
        SetColorScheme colorScheme ->
            ( { model | colorScheme = colorScheme }
            , Effect.none
            )

        SetFormatting formatting ->
            ( { model | formatting = formatting }
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


viewImage : Song -> ReadDirection -> Model -> String -> Int -> File -> Html msg
viewImage song readDirection model readOnlyId index file =
    let
        headerHeight =
            Css.rem 5

        numOfPages =
            List.length song.files
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
            , case readDirection of
                ReadHorizontal ->
                    if numOfPages > 1 then
                        border_r_2

                    else
                        Css.batch []

                ReadVertical ->
                    border_b_2
            ]
        ]
        [ if index == 0 && model.formatting == ShowHeading then
            div
                [ css
                    [ Css.height headerHeight
                    , text_center
                    ]
                ]
                [ h2
                    [ css
                        [ font_sans
                        , font_medium
                        , mb_2
                        ]
                    ]
                    [ text song.name ]
                , p [] [ song.interpreter |> withDefault "" |> text ]
                ]

          else
            text ""
        , div
            [ css <|
                [ flex
                , flex_col

                -- , case model.alignment of
                --     AlignTop ->
                --         justify_start
                --     AlignCenter ->
                --         justify_center
                --     AlignBottom ->
                --         justify_end
                ]
                    ++ (if index == 0 then
                            [ Css.height <|
                                Css.calc (Css.pct 100) Css.minus headerHeight
                            , overflow_hidden
                            ]

                        else
                            [ h_full ]
                       )
            ]
            [ img
                [ src
                    (host
                        ++ "/readonly/"
                        ++ readOnlyId
                        ++ "/tables/files/columns/content/files/rowid/"
                        ++ String.fromInt file.rowid
                    )
                , css <|
                    case readDirection of
                        ReadHorizontal ->
                            [ block

                            -- Prevent wide images from expanding too much
                            , max_w_6xl

                            -- Prevent image from losing aspect ratio
                            , object_contain
                            , if numOfPages > 1 then
                                h_full

                              else
                                Css.batch
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
            (\file ->
                (file.filetype |> Maybe.map String.toLower)
                    == Just (String.toLower filetype)
            )


getSidebar : Model -> Html Msg
getSidebar model =
    let
        colorScheme =
            model.colorScheme

        btnCss =
            Css.batch
                [ cursor_pointer
                , py_1
                , bg_color gray_100
                , Css.hover [ bg_color gray_50 ]
                , Css.active [ bg_color gray_200 ]
                , text_center
                ]

        markSelectedFor : a -> a -> Css.Style
        markSelectedFor reference actual =
            if reference == actual then
                Css.batch
                    [ border_l_4
                    , border_l_color orange_500
                    , border_solid
                    , bg_color gray_300
                    ]

            else
                Css.batch
                    [ border_l_4
                    , border_solid
                    , border_color transparent
                    ]

        formattingButtons =
            [ button
                [ css [ btnCss, markSelectedFor ShowHeading model.formatting ]
                , onClick
                    (if model.formatting == DefaultFormatting then
                        SetFormatting ShowHeading

                     else
                        SetFormatting DefaultFormatting
                    )
                ]
                [ text "H" ]
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

        placeholder =
            [ div [ css [ h_3 ] ] [] ]
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
            , bg_color gray_200
            ]
        ]
        (formattingButtons
            ++ placeholder
            -- ++ alignmentButtons
            -- ++ placeholder
            ++ colorSchemeButtons
        )


viewSong : ReadDirection -> Model -> String -> Song -> Html Msg
viewSong readDirection model readOnlyId song =
    let
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
                                [ flex_row

                                -- , case model.alignment of
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
    if List.isEmpty song.files then
        divCenter [ text "No files" ]

    else if song.files |> filesAreType "pdf" then
        case song.files of
            [ file ] ->
                divImages <|
                    [ iframe
                        [ src
                            (host
                                ++ "/readonly/"
                                ++ readOnlyId
                                ++ "/tables/files/columns/content/files/rowid/"
                                ++ String.fromInt file.rowid
                            )
                        , css [ w_full, h_full, border_none ]
                        ]
                        []
                    ]

            _ :: _ ->
                divCenter [ text "Does not support more than one PDF per song" ]

            _ ->
                divCenter [ text "No files" ]

    else
        divImages <|
            (if readDirection == ReadHorizontal then
                [ getSidebar model ]

             else
                []
            )
                ++ (song.files
                        |> List.indexedMap
                            (viewImage
                                song
                                readDirection
                                model
                                readOnlyId
                            )
                   )


viewPages : Props -> Shared.Model -> Model -> Song -> Html Msg
viewPages settings sharedModel model song =
    let
        readOnlyId =
            sharedModel.readonlyId
                |> Maybe.withDefault ""
    in
    div
        [ css
            [ case settings.readDirection of
                ReadHorizontal ->
                    h_full

                ReadVertical ->
                    if filesAreType "pdf" song.files then
                        h_full

                    else
                        w_full
            , case model.colorScheme of
                Light ->
                    bg_color white

                Dark ->
                    bg_color black

                Sepia ->
                    Css.backgroundColor sepiaColors.bg

            -- Make bg color cover the whole page:
            , overflow_scroll
            ]
        ]
        [ viewSong settings.readDirection model readOnlyId song ]


view :
    Props
    -> Shared.Model
    ->
        { toContentMsg : Msg -> mainMsg
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
                            , body =
                                [ toUnstyled <|
                                    (viewPages settings sharedModel model song
                                        |> Html.Styled.map toContentMsg
                                    )
                                ]
                            }

                        _ ->
                            { title = "Error: Multiple songs"
                            , body = [ Html.text "Error: Multiple songs" ]
                            }

                Nothing ->
                    { title = "Loading …"
                    , body =
                        [ toUnstyled <|
                            div
                                [ css [ text_center, font_sans, pt_8 ] ]
                                [ text "Loading …" ]
                        ]
                    }

        Err httpError ->
            { title = "Error"
            , body = [ toUnstyled <| viewHttpError httpError ]
            }
