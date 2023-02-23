module Utils exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http exposing (Error(..))
import Tailwind.Theme exposing (..)
import Tailwind.Utilities exposing (..)


host : String
host =
    "https://airsequel.fly.dev"


viewHttpError : Http.Error -> Html msg
viewHttpError error =
    let
        errorWrapper txt =
            pre [ css [ p_8, text_color red_800 ] ] [ text txt ]
    in
    case error of
        NetworkError ->
            errorWrapper <| "Network Error"

        Timeout ->
            errorWrapper <| "Timeout"

        BadStatus response ->
            errorWrapper <| ("BadStatus: " ++ String.fromInt response)

        BadBody response ->
            errorWrapper <| "BadPayload: " ++ response

        BadUrl url ->
            errorWrapper <| ("BadUrl: " ++ url)
