module OmniSearch exposing (..)

import Date
import Combine as C exposing ((*>), (<*), (>>=))
import Combine.Num as C

type SearchToken
    = Adults Int
    | ChildAges (List Int)
    | Date Date.Date
    | Other String

parse : String -> List SearchToken
parse input =
    String.words input
        |> List.foldl
            (\word tokens ->
                let
                    res =
                        word |> C.parse
                            (C.choice
                                [ adultsParser
                                , childAgesParser
                                ])
                in
                    case res of
                        Ok (_, _, result) ->
                            result :: tokens
                        _ -> Other word :: tokens
            ) []

safeParseInt : String -> Int
safeParseInt =
    String.trim >> String.toInt >> Result.withDefault 0

adultsParser : C.Parser s SearchToken
adultsParser =
    C.regex "[0-9]{1,2}a"
        |> C.map (String.dropRight 1)
        |> C.map safeParseInt
        |> C.map Adults

intArrayParser : C.Parser s (List Int)
intArrayParser =
    C.sepBy (C.regex " *, *") (C.regex "[0-9]{1,2}")
        |> C.map (List.map safeParseInt)

childAgesParser : C.Parser s SearchToken
childAgesParser =
    C.regex "\\[ *"
        *> intArrayParser
        <* C.regex " *\\]"
        |> C.map ChildAges

dateParser =
    C.many datePartParser

datePartParser =
    C.int <* (C.maybe (C.regex "(-|/)"))
