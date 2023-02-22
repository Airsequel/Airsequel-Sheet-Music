module Pages.Songs.Horizontal.SongId_ exposing (Model, Msg, page)

import Effect exposing (Effect(..))
import GraphQL
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http exposing (Error(..))
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Types.File exposing (File)
import Types.Song exposing (Song)
import Utils exposing (viewHttpError)
import View exposing (View)


page : Shared.Model -> Route { songId : String } -> Page Model Msg
page sharedModel route =
    Page.new
        { init = init sharedModel route
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view sharedModel
        }



-- INIT


type alias Model =
    { songsResult : GraphQL.Response (List Song) }


init : Shared.Model -> Route { songId : String } -> () -> ( Model, Effect Msg )
init sharedModel route _ =
    ( { songsResult = Ok { data = Nothing, errors = Nothing } }
    , case sharedModel.readonlyId of
        Nothing ->
            Effect.none

        Just readonlyId ->
            Shared.getSongWithFiles
                readonlyId
                route.params.songId
                OnSong
    )



-- UPDATE


type Msg
    = OnSong (GraphQL.Response (List Song))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        OnSong songsResult ->
            ( { model | songsResult = songsResult }
            , Effect.none
            )



-- VIEW


viewSong : String -> Song -> Html msg
viewSong sharedModel song =
    div
        [ class "images"
        , style "white-space" "nowrap"
        , style "height" "100%"
        ]
        (song.files |> List.map (viewImage sharedModel))


viewImage : String -> File -> Html msg
viewImage readonlyId file =
    div
        [ class "image"
        , style "display" "inline-block"
        , style "padding" "0.5rem"
        , style "height" "100%"
        ]
        [ img
            [ src
                ("https://airsequel.fly.dev/readonly/"
                    ++ readonlyId
                    ++ "/tables/files/columns/content/files/rowid/"
                    ++ String.fromInt file.rowid
                )
            , style "max-height" "100%"
            , style "max-width" "100%"
            , style "vertical-align " "top"
            , style "border-right-width" "2px"
            , style "border-right-style" "solid"
            , style "border-right-color" "rgb(255, 100, 0)"
            ]
            []
        ]


view : Shared.Model -> Model -> View Msg
view sharedModel model =
    { title = "Pages.Songs.SongId_"
    , body =
        [ toUnstyled <|
            div [ style "height" "100%" ] <|
                case model.songsResult of
                    Ok gqlRes ->
                        case gqlRes.data of
                            Just songs ->
                                case songs.root of
                                    song :: _ ->
                                        [ viewSong
                                            (Maybe.withDefault
                                                ""
                                                sharedModel.readonlyId
                                            )
                                            song
                                        ]

                                    _ ->
                                        [ text "Multiple songs" ]

                            Nothing ->
                                [ text "Loading …" ]

                    Err httpError ->
                        [ viewHttpError httpError ]
        ]
    }
