module Types.FilterOptions exposing (FilterOptions, filterOptionsDecoder)

import Json.Decode as JD exposing (Decoder)
import Set


{-| Distinct filter values over the whole songs collection,
provided by the `filter_options_json` view
(one row per `(field, value)` pair).
-}
type alias FilterOptions =
  { interpreters : List String
  , instrumentations : List String
  , keys : List String
  , tempos : List String
  }


filterOptionsDecoder : Decoder FilterOptions
filterOptionsDecoder =
  JD.list
    (JD.map2
        Tuple.pair
        (JD.field "field" JD.string)
        (JD.field "value" JD.string)
    )
    |> JD.map
        (\pairs ->
            let
              valuesFor fieldName =
                pairs
                  |> List.filterMap
                      (\( field, value ) ->
                          if field == fieldName
                            then Just value
                            else Nothing
                      )
                  |> Set.fromList
                  |> Set.toList
            in
            { interpreters = valuesFor "interpreter"
            , instrumentations = -- Instrumentation values are comma-separated lists
              valuesFor "instrumentation"
                |> List.concatMap (String.split ",")
                |> List.map String.trim
                |> List.filter (not << String.isEmpty)
                |> Set.fromList
                |> Set.toList
            , keys = valuesFor "key"
            , tempos = valuesFor "tempo"
            }
        )
