module Layouts.PlayLayout exposing
    ( Model
    , Msg
    , Settings
    , layout
    )

import Effect exposing (Effect(..))
import GraphQL
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http exposing (Error(..))
import Layout exposing (Layout)
import Route exposing (Route)
import Shared
import Tailwind.Theme exposing (..)
import Tailwind.Utilities exposing (..)
import Types.File exposing (File)
import Types.ReadDirection exposing (ReadDirection(..))
import Types.Song exposing (Song)
import Utils exposing (host, viewHttpError)
import View exposing (View)


type alias Settings =
    { songId : String
    , readDirection : ReadDirection
    , songsResult : GraphQL.Response (List Song)
    }


layout : Settings -> Shared.Model -> Route () -> Layout Model Msg mainMsg
layout settings sharedModel _ =
    Layout.new
        { init = init
        , update = update
        , view = view settings sharedModel
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


viewImage : ReadDirection -> String -> File -> Html msg
viewImage readDirection readonlyId file =
    div
        [ css <|
            case readDirection of
                ReadHorizontal ->
                    [ inline_block, h_full ]

                ReadVertical ->
                    []
        ]
        [ img
            [ src
                (host
                    ++ "/readonly/"
                    ++ readonlyId
                    ++ "/tables/files/columns/content/files/rowid/"
                    ++ String.fromInt file.rowid
                )
            , css
                ([ block
                 , border_solid
                 , border_color orange_500
                 , p_2
                 ]
                    ++ (case readDirection of
                            ReadHorizontal ->
                                [ max_h_full
                                , max_w_full
                                , align_top
                                , border_r_2
                                ]

                            ReadVertical ->
                                [ w_full
                                , h_full
                                , border_b_2
                                ]
                       )
                )
            ]
            []
        ]


filesAreType : String -> List File -> Bool
filesAreType filetype files =
    files
        |> List.all
            (\file ->
                (file.filetype |> Maybe.map String.toLower)
                    == Just (String.toLower filetype)
            )


viewSong : ReadDirection -> String -> Song -> Html msg
viewSong readDirection sharedModel song =
    let
        divImages content =
            div
                [ css [ whitespace_nowrap, h_full ] ]
                content

        divCenter content =
            div
                [ css [ text_center, font_sans, pt_8 ] ]
                content
    in
    if List.isEmpty song.files then
        divCenter [ text "No files" ]

    else if song.files |> filesAreType "pdf" then
        case song.files of
            [ file ] ->
                divImages
                    [ iframe
                        [ src
                            (host
                                ++ "/readonly/"
                                ++ sharedModel
                                ++ "/tables/files/columns/content/files/rowid/"
                                ++ String.fromInt file.rowid
                            )
                        , css [ w_full, h_full, border_none ]
                        ]
                        []
                    ]

            _ :: _ ->
                divCenter [ text "Does not support more than one PDF per song" ]

            _ ->
                divCenter [ text "No files" ]

    else
        divImages
            (song.files
                |> List.map (viewImage readDirection sharedModel)
            )


viewPages : Settings -> Shared.Model -> Html msg
viewPages settings sharedModel =
    case settings.songsResult of
        Ok gqlRes ->
            case gqlRes.data of
                Just songs ->
                    case songs.root of
                        song :: _ ->
                            div
                                [ css
                                    [ bg_color white

                                    -- Make bg color cover the whole page:
                                    , overflow_scroll
                                    , if
                                        (settings.readDirection
                                            == ReadHorizontal
                                        )
                                            || filesAreType "pdf" song.files
                                      then
                                        h_full

                                      else
                                        w_full
                                    ]
                                ]
                                [ viewSong
                                    settings.readDirection
                                    (Maybe.withDefault
                                        ""
                                        sharedModel.readonlyId
                                    )
                                    song
                                ]

                        _ ->
                            div [] [ text "Multiple songs" ]

                Nothing ->
                    div
                        [ css [ text_center, font_sans, pt_8 ] ]
                        [ text "Loading …" ]

        Err httpError ->
            viewHttpError httpError


view :
    Settings
    -> Shared.Model
    ->
        { fromMsg : Msg -> mainMsg
        , content : View mainMsg
        , model : Model
        }
    -> View mainMsg
view settings sharedModel _ =
    { title = "Play View"
    , body =
        [ toUnstyled <|
            viewPages settings sharedModel
        ]
    }
