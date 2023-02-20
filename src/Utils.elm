module Utils exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error(..))


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
