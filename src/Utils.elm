module Utils exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http exposing (Error(..))


host : String
host =
    "https://airsequel.fly.dev"


viewHttpError : Http.Error -> Html msg
viewHttpError error =
    case error of
        NetworkError ->
            span [] [ text "Network Error" ]

        Timeout ->
            span [] [ text "Timeout" ]

        BadStatus response ->
            span []
                [ text
                    ("BadStatus: "
                        ++ String.fromInt response
                    )
                ]

        BadBody response ->
            span [] [ text ("BadPayload: " ++ response) ]

        BadUrl url ->
            span [] [ text ("BadUrl: " ++ url) ]
