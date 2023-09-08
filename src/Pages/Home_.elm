module Pages.Home_ exposing (Model, Msg, page)

import Css
import Css.Global
import Effect exposing (Effect(..))
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput, onSubmit)
import Http exposing (Error(..))
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Msg exposing (Msg(..))
import Svg.Styled as Svg
import Svg.Styled.Attributes exposing (d, viewBox)
import Tailwind.Breakpoints exposing (..)
import Tailwind.Theme exposing (..)
import Tailwind.Utilities exposing (..)
import Types.Song exposing (Song)
import Utils exposing (arrowIconVert, host, viewHttpError)
import View exposing (View)


teamplateDbReadonlyId : String
teamplateDbReadonlyId =
    "6mcw4qrv1wvtwn65"


page : Shared.Model -> Route () -> Page Model Msg
page sharedModel _ =
    Page.new
        { init = init sharedModel
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view sharedModel
        }



-- INIT


type alias Model =
    { sharedModel : Shared.Model
    , partialReadonlyId : String
    , errors : List String
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init sharedModel () =
    ( { sharedModel = sharedModel
      , partialReadonlyId = ""
      , errors = []
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = EnteredReadonlyId String
    | SubmittedReadonlyId


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        EnteredReadonlyId readonlyId ->
            ( { model | partialReadonlyId = readonlyId }
            , Effect.none
            )

        SubmittedReadonlyId ->
            if
                String.length model.partialReadonlyId
                    /= 16
                    && not (String.isEmpty model.partialReadonlyId)
            then
                ( { model
                    | errors = [ """
                        Read-only ID must be 16 characters long.
                        Please make sure you really copied the read-only ID
                        and not the normal database ID.
                        """ ]
                  }
                , Effect.none
                )

            else
                ( { model | partialReadonlyId = "", errors = [] }
                , SendSharedMsg <|
                    Shared.Msg.SubmittedReadonlyId model.partialReadonlyId
                )



-- VIEW


buttonStyle : List Css.Style -> Attribute msg
buttonStyle add =
    css <|
        [ inline_block
        , bg_color blue_200
        , rounded
        , w_6
        , h_6
        ]
            ++ add


viewReadonlyIdForm : Shared.Model -> Model -> Html Msg
viewReadonlyIdForm sharedModel model =
    Html.Styled.form
        [ onSubmit SubmittedReadonlyId
        , css [ inline_block ]
        ]
        [ input
            [ type_ "text"
            , value model.partialReadonlyId
            , placeholder <|
                case sharedModel.readonlyId of
                    Just id ->
                        id

                    Nothing ->
                        "Airsequel read-only ID"
            , onInput EnteredReadonlyId
            , css
                [ inline_block
                , border
                , border_solid
                , border_color gray_400
                , rounded
                , px_2
                , py_1
                ]
            ]
            []
        ]


viewGettingStarted : Shared.Model -> Model -> List (Html Msg)
viewGettingStarted sharedModel model =
    [ h2 [ css [ text_2xl, mb_8 ] ] [ text "Getting Started" ]
    , ol [ css [ list_decimal, ml_8 ] ]
        [ li [ css [ mb_6 ] ]
            [ p [ css [ mb_4 ] ]
                [ text """
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
                    , border_color blue_800
                    , text_color blue_800
                    , max_w_max
                    , bg_color blue_200
                    ]
                ]
                [ text "Create Database" ]
            ]
        , li [ css [ mb_6 ] ]
            [ text """
                Add your songs to the database
                via the songs and files tables in the "Tables" tab.
                """ ]
        , li [ css [ mb_6 ] ]
            [ p [ css [ mb_4 ] ]
                [ text """
                    Find the read-only ID in the "Database" tab,
                    post it here in the input field, and press enter:
                    """
                ]
            , viewReadonlyIdForm sharedModel model
            ]
        ]
    ]


viewSong : Song -> Html msg
viewSong song =
    let
        tdSty additions =
            td
                [ css <|
                    [ border_x_4
                    , border_color white
                    , px_2
                    , py_1
                    ]
                        ++ additions
                ]
    in
    tr
        []
        [ tdSty [] [ text <| Maybe.withDefault "" song.interpreter ]
        , tdSty []
            [ a
                [ href <| "/songs/" ++ String.fromInt song.rowid
                , css [ underline, text_color blue_800 ]
                ]
                [ text song.name ]
            ]
        , tdSty [ px_1 ]
            [ if song.numberOfFiles == 0 then
                text ""

              else
                div
                    [ css
                        [ flex
                        , gap_1
                        , justify_center
                        ]
                    ]
                    [ if song.filetypes == Just "pdf" then
                        span [ buttonStyle [ invisible ] ] [ text "X" ]

                      else
                        a
                            [ href <|
                                "/songs/horizontal/"
                                    ++ String.fromInt song.rowid
                            , buttonStyle [ p_0_dot_5 ]
                            ]
                            [ arrowIconVert [ rotate_90 ] ]
                    , a
                        [ href <|
                            "/songs/vertical/"
                                ++ String.fromInt song.rowid
                        , buttonStyle [ p_0_dot_5 ]
                        ]
                        [ arrowIconVert [] ]
                    ]
            ]
        , tdSty []
            [ text <|
                if song.filetypes == Just "pdf" then
                    ""

                else
                    String.fromInt song.numberOfFiles
            ]
        , tdSty [] [ text <| Maybe.withDefault "" song.instrumentation ]
        , tdSty [ text_center ] [ text <| Maybe.withDefault "" song.tempo ]
        , tdSty [ text_center ] [ text <| Maybe.withDefault "" song.key ]
        ]


viewSongsTable : List Song -> Html Msg
viewSongsTable songs =
    let
        documentIcon styles =
            Svg.svg
                [ viewBox "0 0 24 24"
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
                    , border_color white
                    , px_2
                    , py_1
                    ]
                        ++ additions
                ]

        tableHead =
            thead [] <|
                [ tr [ css [ bg_color blue_100 ] ]
                    [ thSty [] [ text "Interpreter" ]
                    , thSty [] [ text "Song" ]
                    , thSty [] [ text "Open" ]
                    , thSty [ py_0, px_0_dot_5 ]
                        [ documentIcon [ inline_block, h_6 ] ]
                    , thSty [] [ text "Instrumentation" ]
                    , thSty [] [ text "Tempo" ]
                    , thSty [] [ text "Key" ]
                    ]
                ]
    in
    Html.Styled.table
        [ css [ w_full, bg_color white ] ]
        [ tableHead
        , tbody [] <|
            (songs
                |> List.map viewSong
            )
        ]


view : Shared.Model -> Model -> View Msg
view sharedModel model =
    { title = "Airsequel Sheet Music"
    , body =
        [ toUnstyled <|
            main_
                [ css
                    [ bg_color white
                    , max_w_5xl
                    , mx_auto
                    , min_h_full
                    , border_x
                    , border_color gray_400
                    ]
                ]
                [ Css.Global.global globalStyles
                , nav
                    [ css
                        [ px_10
                        , pt_12
                        , pb_4
                        ]
                    ]
                    [ div
                        [ css
                            [ flex
                            , flex_col
                            , sm [ flex_row ]
                            , pb_4
                            ]
                        ]
                        [ h1
                            [ css
                                [ font_bold
                                , text_3xl
                                , mr_4
                                , inline_block
                                , text_color blue_800
                                , grow
                                ]
                            ]
                            [ text "Airsequel Sheet Music" ]
                        , if
                            sharedModel.readonlyId
                                /= Nothing
                                && sharedModel.readonlyId
                                /= Just ""
                          then
                            div
                                [ css [ pt_1_dot_5 ] ]
                                [ label
                                    [ css [ text_color gray_400 ] ]
                                    [ text "Read-Only ID: " ]
                                , viewReadonlyIdForm sharedModel model
                                ]

                          else
                            text ""
                        ]
                    , case sharedModel.songsResult of
                        Ok gqlRes ->
                            case gqlRes.data of
                                Just songsData ->
                                    p [ css [ font_semibold ] ]
                                        [ text <|
                                            "Number of songs: "
                                                ++ String.fromInt
                                                    (List.length songsData.root)
                                        ]

                                Nothing ->
                                    text ""

                        Err _ ->
                            text ""
                    ]
                , div [ css [ overflow_scroll, pb_12, sm [ px_10 ] ] ]
                    (div []
                        (model.errors
                            |> List.map
                                (\error ->
                                    p
                                        [ css
                                            [ bg_color red_200
                                            , border
                                            , border_solid
                                            , border_color red_800
                                            , text_color red_800
                                            , rounded
                                            , px_4
                                            , py_2
                                            , mb_4
                                            , max_w_xl
                                            ]
                                        ]
                                        [ text error ]
                                )
                        )
                        :: (case sharedModel.songsResult of
                                Ok gqlRes ->
                                    case gqlRes.data of
                                        Just songsData ->
                                            [ viewSongsTable songsData.root ]

                                        Nothing ->
                                            if
                                                sharedModel.readonlyId
                                                    == Nothing
                                                    || sharedModel.readonlyId
                                                    == Just ""
                                            then
                                                viewGettingStarted sharedModel model

                                            else
                                                [ div
                                                    [ css [ text_center ] ]
                                                    [ text "Loading …" ]
                                                ]

                                Err httpError ->
                                    [ viewHttpError httpError ]
                           )
                    )
                ]
        ]
    }
