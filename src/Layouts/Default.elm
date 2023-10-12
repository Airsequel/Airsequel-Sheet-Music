module Layouts.Default exposing
    ( Model
    , Msg
    , Props
    , layout
    )

import Css.Global
import Effect exposing (Effect)
import Html.Styled exposing (a, div, h1, main_, nav, text, toUnstyled)
import Html.Styled.Attributes exposing (..)
import Layout exposing (Layout)
import Route exposing (Route)
import Shared
import Tailwind.Breakpoints exposing (..)
import Tailwind.Theme exposing (..)
import Tailwind.Utilities exposing (..)
import View exposing (View)


type alias Props =
    { title : String }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout settings _ _ =
    Layout.new
        { init = init
        , update = update
        , view = view settings
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


view :
    Props
    ->
        { toContentMsg : Msg -> mainMsg
        , content : View mainMsg
        , model : Model
        }
    -> View mainMsg
view settings { content } =
    { title = settings.title
    , body =
        [ toUnstyled <|
            main_
                [ css
                    [ bg_color white
                    , py_12
                    , px_10
                    , max_w_5xl
                    , mx_auto
                    , min_h_full
                    , border_color gray_400
                    , lg [ border_x ]
                    ]
                ]
                [ Css.Global.global globalStyles
                , nav
                    [ css
                        [ flex
                        , flex_col
                        , sm [ flex_row ]
                        , pb_8
                        ]
                    ]
                    [ h1
                        [ css
                            [ font_bold
                            , text_3xl
                            , mr_4
                            , inline_block
                            , text_color blue_800
                            , grow
                            ]
                        ]
                        [ a [ href "/" ]
                            [ text "Airsequel Sheet Music" ]
                        ]
                    ]
                , div []
                    (content.body
                        |> List.map Html.Styled.fromUnstyled
                    )
                ]
        ]
    }
