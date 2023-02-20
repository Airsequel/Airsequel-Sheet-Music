module Pages.Songs.Vertical.SongId_ exposing (Model, Msg, page)

import Config
import GraphQL
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error(..))
import Page exposing (Page)
import Types.File exposing (File)
import Types.Song exposing (Song, getSongsWithFiles)
import Utils exposing (viewHttpError)
import View exposing (View)


viewSong : Song -> Html msg
viewSong song =
    div
        [ class "images" ]
        (song.files |> List.map viewImage)


viewImage : File -> Html msg
viewImage file =
    div
        [ class "image" ]
        [ img
            [ src
                (Config.dbUrl
                    ++ "/tables/files/columns/content/files/rowid/"
                    ++ String.fromInt file.rowid
                )
            , style "width" "100%"
            , style "max-height" "100%"
            ]
            []
        ]


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


view : Model -> View Msg
view model =
    { title = "Pages.Songs.SongId_"
    , body =
        [ div [ style "width" "100%" ] <|
            case model.songsResult of
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

                Err httpError ->
                    [ viewHttpError httpError ]
        ]
    }
