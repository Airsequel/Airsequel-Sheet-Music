module Pages.Songs.SongId_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import GraphQL
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http exposing (Error(..))
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Tailwind.Breakpoints exposing (..)
import Tailwind.Theme exposing (..)
import Tailwind.Utilities exposing (..)
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
        |> Page.withLayout
            (\_ ->
                Layouts.Default
                    { default =
                        { title =
                            "Song " ++ route.params.songId
                        }
                    }
            )



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
viewSong _ song =
    let
        buttonCss =
            css
                [ inline_block
                , border
                , rounded
                , px_2
                , py_1
                , bg_color blue_200
                , mr_2
                , no_underline
                , text_color blue_800
                , border
                , border_solid
                , border_color blue_800
                ]

        keySpan value =
            span
                [ css [ inline_block, w_40, text_right, mr_2 ] ]
                [ text value ]
    in
    div []
        [ h2 [ css [ text_3xl, mb_8 ] ] [ text song.name ]
        , ul [ css [ mb_8 ] ]
            [ li []
                [ keySpan "Interpreter:"
                , strong []
                    [ text <| Maybe.withDefault "-" song.interpreter ]
                ]
            , li []
                [ keySpan "Instrumentation:"
                , strong []
                    [ text <| Maybe.withDefault "-" song.instrumentation ]
                ]
            , li []
                [ keySpan "Tempo:"
                , strong []
                    [ text <| Maybe.withDefault "-" song.tempo ]
                ]
            , li []
                [ keySpan "Key:"
                , strong []
                    [ text <| Maybe.withDefault "-" song.key ]
                ]
            , li []
                [ keySpan "Number of Pages: "
                , strong []
                    [ text <| String.fromInt <| List.length song.files ]
                ]
            ]
        , if List.isEmpty song.files then
            text ""

          else
            div []
                [ a
                    [ href ("/songs/horizontal/" ++ String.fromInt song.rowid)
                    , buttonCss
                    ]
                    [ text "↔ Horizontal View" ]
                , a
                    [ href ("/songs/vertical/" ++ String.fromInt song.rowid)
                    , buttonCss
                    ]
                    [ text "↕ Vertical View" ]
                ]
        ]


view : Shared.Model -> Model -> View Msg
view sharedModel model =
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

                    Err error ->
                        [ viewHttpError error ]
                )
        ]
    }
