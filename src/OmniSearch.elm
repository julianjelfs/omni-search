module OmniSearch exposing (..)

import Date
import Combine as C exposing ((*>), (<*), (>>=), (<|>))
import Combine.Num as C
import Combine.Char as C

word = C.regex "[A-Za-z]+"

type SearchToken
    = Adults Int
    | ChildAges (List Int)
    | Date Date.Date
    | From String
    | To String
    | Other String

parse : String -> List SearchToken -> List SearchToken
parse txt tokens =
    case txt of
        "" -> tokens
        _ ->
            let
                res =
                   txt |> C.parse
                        (C.choice
                            [ adultsParser
                            , childAgesParser
                            , fromParser
                            , toParser
                            , wordParser
                            ])
            in
                case res of
                    Ok (_, { input }, result) ->
                        parse input (result :: tokens)
                    _ ->
                        case C.parse C.space txt of
                            Ok (_, { input }, result) ->
                                parse input tokens
                            _ -> tokens

safeParseInt : String -> Int
safeParseInt =
    String.trim >> String.toInt >> Result.withDefault 0

wordParser : C.Parser s SearchToken
wordParser =
    word |> C.map Other

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

fullDateParser =
    C.regex "[0-9]{1,2}-[0-9]{1,2}-[0-9]{2,4}"
        <|> C.regex "[0-9]{1,2}//[0-9]{1,2}//[0-9]{2,4}"

partialDateParser =
    C.regex "[0-9]{1,2}-[0-9]{1,2}"
        <|> C.regex "[0-9]{1,2}//[0-9]{1,2}"

datePartParser =
    C.int <* (C.maybe (C.regex "(-|/)"))

fromParser =
    C.string "from " *> word
        |> C.map From

toParser =
    C.string "to " *> word
        |> C.map To
