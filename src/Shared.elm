module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    , getSongWithFiles
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Effect exposing (Effect)
import GraphQL
import Json.Decode
import Route exposing (Route)
import Shared.Model
import Shared.Msg
import Types.Song exposing (songsDecoder)


getSongs : String -> Effect Msg
getSongs readonlyId =
    Effect.sendCmd <|
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
            , url =
                "https://airsequel.fly.dev/readonly/"
                    ++ readonlyId
                    ++ "/graphql"
            , headers = []
            , on = Shared.Msg.OnSongs
            , variables = Nothing
            }


getSongWithFiles :
    String
    -> String
    -> (GraphQL.Response (List Types.Song.Song) -> msg)
    -> Effect msg
getSongWithFiles readonlyId songId msg =
    Effect.sendCmd <|
        GraphQL.run
            { query = """
            query SongsWithFiles {
                songs_with_files_json (
                    filter: { rowid: { eq: """ ++ songId ++ """ } }
                ) {
                    rowid
                    name
                    instrumentation
                    tempo
                    key
                    interpreter
                    files
                }
            }
            """
            , decoder = songsDecoder True
            , root = "songs_with_files_json"
            , url =
                "https://airsequel.fly.dev/readonly/"
                    ++ readonlyId
                    ++ "/graphql"
            , headers = []
            , on = msg
            , variables = Nothing
            }



-- FLAGS


type alias Flags =
    { readonlyId : Maybe String }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map Flags
        (Json.Decode.field "readonlyId" (Json.Decode.maybe Json.Decode.string))



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult _ =
    let
        emptyModel =
            { readonlyId = Nothing
            , songsResult =
                Ok
                    { data = Nothing
                    , errors = Nothing
                    }
            }
    in
    case flagsResult of
        Ok flags ->
            case flags.readonlyId of
                Just readonlyId ->
                    ( { emptyModel | readonlyId = Just readonlyId }
                    , getSongs readonlyId
                    )

                Nothing ->
                    ( emptyModel, Effect.none )

        Err _ ->
            ( emptyModel, Effect.none )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        Shared.Msg.SubmittedReadonlyId readonlyId ->
            ( { model
                | readonlyId = Just readonlyId
                , songsResult = Ok { data = Nothing, errors = Nothing }
              }
            , Effect.batch
                [ Effect.saveReadonlyId readonlyId
                , getSongs readonlyId
                ]
            )

        Shared.Msg.OnSongs songsResult ->
            ( { model | songsResult = songsResult }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
