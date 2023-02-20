module Pages.Home_ exposing (Model, Msg, page)

import GraphQL
import Html exposing (div, main_, span, text)
import Http exposing (Error(..))
import Json.Decode exposing (Decoder)
import Page exposing (Page)
import View exposing (View)


type alias Song =
    { name : String
    , instrumentation : Maybe String
    , tempo : Maybe String
    , key : Maybe String
    , interpreter : Maybe String
    }


page : Page Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


songDecoder : Decoder Song
songDecoder =
    Json.Decode.map5 Song
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "instrumentation"
            (Json.Decode.maybe Json.Decode.string)
        )
        (Json.Decode.field "tempo" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "key" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "interpreter"
            (Json.Decode.maybe Json.Decode.string)
        )


songsDecoder : Decoder (List Song)
songsDecoder =
    Json.Decode.list songDecoder


getSongs : Cmd Msg
getSongs =
    GraphQL.run
        { query = """
            query {
                songs_with_files {
                    name
                    instrumentation
                    tempo
                    key
                    interpreter
                }
            }
            """
        , decoder = songsDecoder
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
    { title = "Pages.Home_"
    , body =
        [ main_ []
            (case model.songsResult of
                Ok gqlRes ->
                    case gqlRes.data of
                        Just songs ->
                            songs.root
                                |> List.map viewSong

                        Nothing ->
                            [ text "No songs" ]

                Err httpError ->
                    case httpError of
                        BadUrl url ->
                            [ text ("BadUrl: " ++ url) ]

                        Timeout ->
                            [ text "Timeout" ]

                        NetworkError ->
                            [ text "NetworkError" ]

                        BadStatus response ->
                            [ text
                                ("BadStatus: "
                                    ++ String.fromInt response
                                )
                            ]

                        BadBody response ->
                            [ text ("BadPayload: " ++ response) ]
            )
        ]
    }


viewSong : Song -> Html.Html msg
viewSong song =
    div []
        [ div []
            [ span [] [ text song.name ]
            , span [] [ text <| Maybe.withDefault "" song.instrumentation ]
            , span [] [ text <| Maybe.withDefault "" song.tempo ]
            , span [] [ text <| Maybe.withDefault "" song.key ]
            , span [] [ text <| Maybe.withDefault "" song.interpreter ]
            ]
        ]
