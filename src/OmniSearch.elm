module OmniSearch exposing (..)

import Date
import Combine as C exposing ((*>), (<*), (>>=))
import Combine.Num as C
import Combine.Char as C

type SearchToken
    = Adults Int
    | ChildAges (List Int)
    | Date Date.Date
    | Other String

{-|
I have a feeling this is exactly what chainl is for but I don't quite get it
-}
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
    C.regex "[A-Za-z]+"
        |> C.map Other

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