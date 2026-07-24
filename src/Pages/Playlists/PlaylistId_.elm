module Pages.Playlists.PlaylistId_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import GraphQL
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import SongsTable
import Tailwind.Utilities exposing (..)
import Theme exposing (Theme)
import Types.Playlist exposing (Playlist)
import Types.Song exposing (Song)
import Utils exposing (columnFileUrl, viewGraphQLErrors, viewHttpError)
import View exposing (View)


page : Shared.Model -> Route { playlistId : String } -> Page Model Msg
page sharedModel route =
  Page.new
    { init = init sharedModel route
    , update = update sharedModel
    , subscriptions = \_ -> Sub.none
    , view = view sharedModel
    }
    |> Page.withLayout
        (\_ -> Layouts.Default { title = "Playlist" })


-- INIT
type alias Model =
  { playlistId : String
  , playlist : Maybe Playlist
  , playlistResult : GraphQL.Response (List Playlist)
  , songsResult : GraphQL.Response (List Song)
  , songsLoading : Bool
  }


init :
  Shared.Model
  -> Route { playlistId : String }
  -> ()
  -> ( Model, Effect Msg )
init sharedModel route () =
  ( { playlistId = route.params.playlistId
    , playlist = Nothing
    , playlistResult = Ok { data = Nothing, errors = Nothing }
    , songsResult = Ok { data = Nothing, errors = Nothing }
    , songsLoading = False
    }
  , case sharedModel.readonlyId of
      Nothing ->
        Effect.none
      Just readonlyId ->
        Shared.getPlaylist readonlyId route.params.playlistId OnPlaylist
  )


-- UPDATE
type Msg
  = OnPlaylist (GraphQL.Response (List Playlist))
  | OnSongs (GraphQL.Response (List Song))


{-| The first row of a `Result`-wrapped GraphQL list response.
-}
firstRow : GraphQL.Response (List a) -> Maybe a
firstRow result =
  case result of
    Ok res ->
      res.data |> Maybe.andThen (.root >> List.head)
    Err _ ->
      Nothing


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update sharedModel msg model =
  case msg of
    OnPlaylist result ->
      case firstRow result of
        Just playlist ->
          if List.isEmpty playlist.songIds
            then
              ( { model
                  | playlist = Just playlist
                  , playlistResult = result
                }
              , Effect.none
              )
            else
              ( { model
                  | playlist = Just playlist
                  , playlistResult = result
                  , songsLoading = True
                }
              , case sharedModel.readonlyId of
                  Just readonlyId ->
                    Shared.getSongsByIds readonlyId playlist.songIds OnSongs
                  Nothing ->
                    Effect.none
              )
        Nothing ->
          ( { model | playlistResult = result }, Effect.none )
    OnSongs result ->
      ( { model | songsResult = result, songsLoading = False }
      , Effect.none
      )


-- VIEW

{-| Reorder the fetched songs to follow the playlist's own order,
dropping any ids that no longer resolve to a song.
-}
orderedSongs : Playlist -> List Song -> List Song
orderedSongs playlist songs =
  playlist.songIds
    |> List.filterMap
        (\songId -> songs
            |> List.filter (\song -> song.rowid == songId)
            |> List.head
        )


viewHeader : Theme -> String -> Playlist -> Html Msg
viewHeader theme readonlyId playlist =
  let
    songCount =
      List.length playlist.songIds

    cover =
      if playlist.hasCoverImage
        then img
          [ src
              (columnFileUrl
                  readonlyId
                  "playlists"
                  "cover_image"
                  playlist.rowid
              )
          , alt ("Cover of " ++ playlist.name)
          , css
              [ w_20
              , h_20
              , object_cover
              , rounded
              , flex_none
              ]
          ]
          []
        else text ""
  in
  div
    [ css [ flex, flex_col, gap_2, mb_6 ] ]
    [ a
        [ href "/playlists"
        , css [ underline, text_color theme.textLink, text_sm ]
        ]
        [ text "← Playlists" ]
    , div
        [ css [ flex, items_center, gap_4 ] ]
        [ cover
        , div
            []
            [ h1
                [ css [ font_bold, text_2xl, text_color theme.textPrimary ] ]
                [ text playlist.name ]
            , p
                [ css [ text_sm, text_color theme.textMuted, mt_1 ] ]
                [ text
                    (String.fromInt songCount
                      ++ (if songCount == 1
                          then " song"
                          else " songs"
                      )
                    )
                ]
            ]
        ]
    ]


viewSongsSection : Theme -> Model -> Playlist -> Html Msg
viewSongsSection theme model playlist =
  if List.isEmpty playlist.songIds
    then p
      [ css [ text_color theme.textMuted, py_4 ] ]
      [ text "This playlist has no songs yet." ]
    else
      if model.songsLoading
        then div
          [ css [ text_center, py_8, text_color theme.textMuted ] ]
          [ text "Loading songs …" ]
        else case model.songsResult of
          Ok gqlRes ->
            case gqlRes.data of
              Just songsData ->
                SongsTable.viewTable
                  theme
                  (orderedSongs playlist songsData.root)
              Nothing ->
                case gqlRes.errors of
                  Just gqlErrors ->
                    viewGraphQLErrors gqlErrors
                  Nothing ->
                    div
                      [ css [ text_center, py_8, text_color theme.textMuted ] ]
                      [ text "Loading songs …" ]
          Err httpError ->
            viewHttpError httpError


view : Shared.Model -> Model -> View Msg
view sharedModel model =
  let
    darkMode =
      Shared.Model.isDark sharedModel

    theme =
      Theme.fromDarkMode darkMode

    readonlyIdEmpty =
      (sharedModel.readonlyId == Nothing)
      || (sharedModel.readonlyId == Just "")

    body =
      if readonlyIdEmpty
        then [ p
            [ css [ text_color theme.textMuted, py_8 ] ]
            [ text "Set a read-only database ID on the "
            , a
                [ href "/"
                , css [ underline, text_color theme.textLink ]
                ]
                [ text "songs page" ]
            , text " to load playlists."
            ]
        ]
        else case model.playlist of
          Just playlist ->
            [ viewHeader
                theme
                (Maybe.withDefault "" sharedModel.readonlyId)
                playlist
            , viewSongsSection theme model playlist
            ]
          Nothing ->
            case model.playlistResult of
              Ok gqlRes ->
                case gqlRes.errors of
                  Just gqlErrors ->
                    [ viewGraphQLErrors gqlErrors ]
                  Nothing ->
                    case gqlRes.data of
                      Just _ ->
                        [ p
                            [ css [ text_color theme.textMuted, py_8 ] ]
                            [ text "Playlist not found." ]
                        ]
                      Nothing ->
                        [ div
                            [ css
                                [ text_center
                                , py_8
                                , text_color theme.textMuted
                                ]
                            ]
                            [ text "Loading …" ]
                        ]
              Err httpError ->
                [ viewHttpError httpError ]
  in
  { title = "Playlist — Airsequel Sheet Music"
  , body = [ toUnstyled <|
        div [ css [ text_color theme.textPrimary ] ] body
    ]
  }
