module Pages.Home_ exposing (Model, Msg, page)

import Config exposing (dbUrl)
import Css
import Css.Global
import GraphQL
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http exposing (Error(..))
import Page exposing (Page)
import Tailwind.Breakpoints exposing (..)
import Tailwind.Theme exposing (..)
import Tailwind.Utilities exposing (..)
import Types.Song exposing (Song, songsDecoder)
import Utils exposing (viewHttpError)
import View exposing (View)


page : Page Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


getSongs : Cmd Msg
getSongs =
    GraphQL.run
        { query = """
            query SongsWithFiles {
                songs_with_files {
                    rowid
                    name
                    instrumentation
                    tempo
                    key
                    interpreter
                }
            }
            """
        , decoder = songsDecoder False
        , root = "songs_with_files"
        , url = dbUrl ++ "/graphql"
        , headers = []
        , on = OnSongs
        , variables = Nothing
        }



-- INIT


type alias Model =
    { songsResult : GraphQL.Response (List Song) }


init : ( Model, Cmd Msg )
init =
    ( { songsResult =
            Ok
                { data = Nothing
                , errors = Nothing
                }
      }
    , getSongs
    )



-- UPDATE


type Msg
    = ExampleMsgReplaceMe
    | OnSongs (GraphQL.Response (List Song))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExampleMsgReplaceMe ->
            ( model
            , Cmd.none
            )

        OnSongs songsResult ->
            ( { model | songsResult = songsResult }
            , Cmd.none
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


view : Model -> View Msg
view model =
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
                        , input
                            [ type_ "text"
                            , placeholder "Airsequel Readonly ID"
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
                    , div []
                        (case model.songsResult of
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
