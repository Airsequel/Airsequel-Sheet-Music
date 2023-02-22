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
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init sharedModel () =
    ( { sharedModel = sharedModel
      , partialReadonlyId = ""
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
            ( { model | partialReadonlyId = "" }
            , SendSharedMsg <|
                Shared.Msg.SubmittedReadonlyId model.partialReadonlyId
            )



-- VIEW


viewSong : Song -> Html msg
viewSong song =
    let
        tdSty additions =
            td
                [ css <|
                    [ border
                    , border_color slate_500
                    , px_2
                    , py_1
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
        , tdSty []
            (let
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
             in
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
            )
        , tdSty [] [ text <| Maybe.withDefault "" song.instrumentation ]
        , tdSty [] [ text <| Maybe.withDefault "" song.tempo ]
        , tdSty [] [ text <| Maybe.withDefault "" song.key ]
        , tdSty [] [ text <| Maybe.withDefault "" song.interpreter ]
        ]


view : Shared.Model -> Model -> View Msg
view sharedModel model =
    let
        thSty additions =
            th
                [ css <|
                    [ border
                    , border_color slate_500
                    , px_2
                    , py_1
                    ]
                        ++ additions
                ]

        tableHead =
            thead [] <|
                [ tr [ css [ bg_color blue_100 ] ]
                    [ thSty [] [ text "Name" ]
                    , thSty [] [ text "Open" ]
                    , thSty [] [ text "Instrumentation" ]
                    , thSty [] [ text "Tempo" ]
                    , thSty [] [ text "Key" ]
                    , thSty [] [ text "Interpreter" ]
                    ]
                ]

        viewSongsTable songs =
            Html.Styled.table
                [ css [ w_full ] ]
                [ tableHead
                , tbody [] <|
                    (songs.root
                        |> List.map viewSong
                    )
                ]
    in
    { title = "Pages.Home_"
    , body =
        [ toUnstyled <|
            main_
                [ css
                    [ bg_color white
                    , py_16
                    , px_10
                    , max_w_4xl
                    , mx_auto
                    , min_h_full
                    , border_x
                    , border_color gray_400
                    ]
                ]
                [ Css.Global.global globalStyles
                , div
                    []
                    [ nav
                        []
                        [ h1
                            [ css
                                [ font_bold
                                , text_2xl
                                , mb_8
                                , mr_4
                                , inline_block
                                ]
                            ]
                            [ text "Airsequel Sheet Music" ]
                        , Html.Styled.form
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
                        ]
                    , div []
                        (case sharedModel.songsResult of
                            Ok gqlRes ->
                                case gqlRes.data of
                                    Just songs ->
                                        [ viewSongsTable songs ]

                                    Nothing ->
                                        [ div
                                            [ style "text-align" "center" ]
                                            [ text "Loading …" ]
                                        ]

                            Err httpError ->
                                [ viewHttpError httpError ]
                        )
                    ]
                ]
        ]
    }
