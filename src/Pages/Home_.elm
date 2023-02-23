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
import Tailwind.Breakpoints exposing (..)
import Tailwind.Theme exposing (..)
import Tailwind.Utilities exposing (..)
import Types.Song exposing (Song)
import Utils exposing (viewHttpError)
import View exposing (View)


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
                        Readonly ID must be 16 characters long.
                        Please make sure you really copied the readonly ID
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
        , mr_1
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
                        "Airsequel Readonly ID"
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
                    from our template database:
                    """
                ]
            , a
                [ href
                    "https://airsequel.fly.dev/readonly/0wxf09fh50yss0gk/duplicate"
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
                via the songs and files tables.
                """ ]
        , li [ css [ mb_6 ] ]
            [ p [ css [ mb_4 ] ]
                [ text """
                    Copy the readonly ID of your newly created database
                    and post it in the input field:
                    """
                ]
            , viewReadonlyIdForm sharedModel model
            ]
        , li [ css [ mb_6 ] ]
            [ text "Start making music! 🎶🎉" ]
        ]
    ]


viewSong : Song -> Html msg
viewSong song =
    let
        tdSty additions =
            td
                [ css <|
                    [ border_4
                    , border_color white
                    , px_2
                    , py_0_dot_5
                    ]
                        ++ additions
                ]
    in
    tr
        [ css
            [ Css.pseudoClass "odd" [ bg_color white ]
            , Css.pseudoClass "even" [ bg_color gray_200 ]
            ]
        ]
        [ tdSty []
            [ a
                [ href <| "/songs/" ++ String.fromInt song.rowid
                , css [ underline, text_color blue_800 ]
                ]
                [ text song.name ]
            ]
        , tdSty [] [ text <| Maybe.withDefault "" song.interpreter ]
        , tdSty []
            [ a
                [ href <| "/songs/horizontal/" ++ String.fromInt song.rowid
                , buttonStyle [ px_1 ]
                ]
                [ text "↔" ]
            , a
                [ href <| "/songs/vertical/" ++ String.fromInt song.rowid
                , buttonStyle [ px_2 ]
                ]
                [ text "↕" ]
            ]
        , tdSty [] [ text <| Maybe.withDefault "" song.instrumentation ]
        , tdSty [] [ text <| Maybe.withDefault "" song.tempo ]
        , tdSty [] [ text <| Maybe.withDefault "" song.key ]
        ]


viewSongsTable : List Song -> Html Msg
viewSongsTable songs =
    let
        thSty additions =
            th
                [ css <|
                    [ border_4
                    , border_color white
                    , px_2
                    , py_1
                    ]
                        ++ additions
                ]

        tableHead =
            thead [] <|
                [ tr [ css [ bg_color blue_100 ] ]
                    [ thSty [] [ text "Song" ]
                    , thSty [] [ text "Interpreter" ]
                    , thSty [] [ text "Open" ]
                    , thSty [] [ text "Instrumentation" ]
                    , thSty [] [ text "Tempo" ]
                    , thSty [] [ text "Key" ]
                    ]
                ]
    in
    Html.Styled.table
        [ css [ w_full ] ]
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
                    , py_12
                    , px_10
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
                        [ flex
                        , flex_col
                        , sm [ flex_row ]
                        , pb_8
                        ]
                    ]
                    (h1
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
                        :: (if
                                sharedModel.readonlyId
                                    /= Nothing
                                    && sharedModel.readonlyId
                                    /= Just ""
                            then
                                [ div [ css [ pt_1_dot_5 ] ]
                                    [ label
                                        [ css [ text_color gray_400 ] ]
                                        [ text "Readonly ID: " ]
                                    , viewReadonlyIdForm sharedModel model
                                    ]
                                ]

                            else
                                []
                           )
                    )
                , div []
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
                                                    [ style "text-align"
                                                        "center"
                                                    ]
                                                    [ text "Loading …" ]
                                                ]

                                Err httpError ->
                                    [ viewHttpError httpError ]
                           )
                    )
                ]
        ]
    }
