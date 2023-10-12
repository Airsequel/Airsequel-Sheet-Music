module Utils exposing (addStarIf, arrowIconVert, host, viewHttpError)

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http exposing (Error(..))
import Svg.Styled as Svg
import Svg.Styled.Attributes exposing (d, viewBox)
import Tailwind.Theme exposing (..)
import Tailwind.Utilities exposing (..)


host : String
host =
    "https://www.airsequel.com"


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


arrowIconVert : List Css.Style -> Html msg
arrowIconVert styles =
    Svg.svg
        [ viewBox "0 0 24 24"
        , css <| [ block, w_full, h_full ] ++ styles
        ]
        [ Svg.path
            [ d <|
                "M13 6.99h3L12 3 8 6.99h3v10.02"
                    ++ "H8L12 21l4-3.99h-3z"
            ]
            []
        ]


addStarIf : Bool -> String
addStarIf condition =
    if condition then
        "⭐️ "

    else
        ""
