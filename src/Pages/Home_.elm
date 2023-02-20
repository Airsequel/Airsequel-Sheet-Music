module Pages.Home_ exposing (Model, Msg, page)

import GraphQL
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error(..))
import Page exposing (Page)
import Types.Song exposing (Song, songsDecoder)
import Utils exposing (viewHttpError)
import View exposing (View)


page : Page Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
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
        , url = "https://airsequel.fly.dev/readonly/270ny11gtb74f00k/graphql"
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    let
        tableHead =
            thead [] <|
                [ tr []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Instrumentation" ]
                    , th [] [ text "Tempo" ]
                    , th [] [ text "Key" ]
                    , th [] [ text "Interpreter" ]
                    ]
                ]
    in
    { title = "Pages.Home_"
    , body =
        [ main_
            []
            [ div [ style "max-width" "50rem", style "margin" "0 auto" ]
                [ h1 [ style "margin-bottom" "2rem" ]
                    [ text "Airsequel Sheetmusic" ]
                , div []
                    (case model.songsResult of
                        Ok gqlRes ->
                            case gqlRes.data of
                                Just songs ->
                                    [ table [ style "width" "100%" ] <|
                                        [ tableHead
                                        , tbody [] <|
                                            (songs.root
                                                |> List.map viewSong
                                            )
                                        ]
                                    ]

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


viewSong : Song -> Html.Html msg
viewSong song =
    tr []
        [ td []
            [ a
                [ href <| "/songs/" ++ String.fromInt song.rowid ]
                [ text song.name ]
            ]
        , td [] [ text <| Maybe.withDefault "" song.instrumentation ]
        , td [] [ text <| Maybe.withDefault "" song.tempo ]
        , td [] [ text <| Maybe.withDefault "" song.key ]
        , td [] [ text <| Maybe.withDefault "" song.interpreter ]
        ]
