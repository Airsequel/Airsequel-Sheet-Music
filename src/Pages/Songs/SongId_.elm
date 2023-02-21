module Pages.Songs.SongId_ exposing (Model, Msg, page)

import Config
import GraphQL
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http exposing (Error(..))
import Page exposing (Page)
import Types.File exposing (File)
import Types.Song exposing (Song, getSongsWithFiles)
import Utils exposing (viewHttpError)
import View exposing (View)


page : { songId : String } -> Page Model Msg
page params =
    Page.element
        { init = init params
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- INIT


type alias Model =
    { songsResult : GraphQL.Response (List Song) }


init : { songId : String } -> ( Model, Cmd Msg )
init params =
    ( { songsResult = Ok { data = Nothing, errors = Nothing } }
    , getSongsWithFiles params.songId OnSong
    )



-- UPDATE


type Msg
    = OnSong (GraphQL.Response (List Song))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnSong songsResult ->
            ( { model | songsResult = songsResult }
            , Cmd.none
            )



-- VIEW


viewImage : File -> Html msg
viewImage file =
    div
        [ class "image"
        , style "display" "inline-block"
        ]
        [ img
            [ src
                (Config.dbUrl
                    ++ "/tables/files/columns/content/files/rowid/"
                    ++ String.fromInt file.rowid
                )
            , style "max-height" "25rem"
            , style "max-width" "20rem"
            ]
            []
        ]


viewSong : Song -> Html msg
viewSong song =
    div []
        [ a [ href "/" ] [ text "← Back to Songs" ]
        , h2 [] [ text song.name ]
        , ul []
            [ li []
                [ text "Interpreter: "
                , strong []
                    [ text <| Maybe.withDefault "" song.interpreter ]
                ]
            , li []
                [ text "Instrumentation: "
                , strong []
                    [ text <| Maybe.withDefault "" song.instrumentation ]
                ]
            , li []
                [ text "Tempo: "
                , strong []
                    [ text <| Maybe.withDefault "" song.tempo ]
                ]
            , li []
                [ text "Key: "
                , strong []
                    [ text <| Maybe.withDefault "" song.key ]
                ]
            ]
        , a
            [ href ("/songs/horizontal/" ++ String.fromInt song.rowid)
            , class "button"
            ]
            [ text "Horizontal View" ]
        , a
            [ href ("/songs/vertical/" ++ String.fromInt song.rowid)
            , class "button"
            ]
            [ text "Vertical View" ]
        , br [] []
        , div [ class "images" ]
            [ h3 [] [ text "Images" ]
            , br [] []
            , div []
                (song.files |> List.map viewImage)
            ]
        ]


view : Model -> View Msg
view model =
    { title = "Pages.Songs.SongId_"
    , body =
        [ toUnstyled <|
            div []
                (case model.songsResult of
                    Ok gqlRes ->
                        case gqlRes.data of
                            Just songs ->
                                case songs.root of
                                    song :: _ ->
                                        [ viewSong song ]

                                    _ ->
                                        [ text "Multiple songs" ]

                            Nothing ->
                                [ text "Loading …" ]

                    Err error ->
                        [ viewHttpError error ]
                )
        ]
    }
