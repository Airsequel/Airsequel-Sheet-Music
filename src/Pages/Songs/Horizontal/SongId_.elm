module Pages.Songs.Horizontal.SongId_ exposing (Model, Msg, page)

import Effect exposing (Effect(..))
import GraphQL
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http exposing (Error(..))
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Tailwind.Utilities exposing (..)
import Types.ReadDirection exposing (ReadDirection(..))
import Types.Song exposing (Song)
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
            (\model ->
                Layouts.PlayLayout
                    { songId = route.params.songId
                    , readDirection = ReadHorizontal
                    , songsResult = model.songsResult
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


view : Shared.Model -> Model -> View Msg
view _ _ =
    { title = "Horizontal Song View"
    , body = []
    }
