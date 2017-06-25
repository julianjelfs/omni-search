module OmniSearch exposing (..)

import Date
import Combine as C exposing ((*>), (<*), (>>=), (<|>), (<$>))
import Combine.Num as C
import Combine.Char as C
import Date.Extra.Create as D
import Date.Extra.Core as D

word = C.regex "[A-Za-z]+"

type SearchToken
    = Adults Int
    | ChildAges (List Int)
    | Date Date.Date
    | From String
    | To String
    | Nights Int
    | Other String

type DurationType
    = Week
    | Day

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
                            , dateParser
                            , durationParser
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

wordParser : C.Parser s SearchToken
wordParser =
    word |> C.map Other

adultsParser : C.Parser s SearchToken
adultsParser =
    C.int <* C.regex " adult(s)?"
        |> C.map Adults

intArrayParser : C.Parser s (List Int)
intArrayParser =
    C.sepBy (C.regex " *, *") C.int

childAgesParser : C.Parser s SearchToken
childAgesParser =
    C.regex "\\[ *"
        *> intArrayParser
        <* C.regex " *\\]"
        |> C.map ChildAges

dateParser =
    clamp 1 31 <$> datePartParser
        >>= (\d -> clamp 1 12 <$> datePartParser
        >>= (\m -> datePartParser
        |> C.map (\y ->
            Date <| D.dateFromFields y (D.intToMonth m) d 0 0 0 0)))

datePartParser =
    C.regex "[0-9]+" <* (C.maybe (C.regex "(-|/)"))
        |> C.map safeStringToInt

fromParser =
    C.string "from " *> word
        |> C.map From

toParser =
    C.string "to " *> word
        |> C.map To

safeStringToInt =
    String.toInt >> Result.withDefault 0

durationParser =
    safeStringToInt <$> C.regex "[0-9]+"
        >>= (\n -> durationDescriptionParser
        |> C.map
            (\d ->
                case d of
                    Week -> Nights (n * 7)
                    Day -> Nights n
            ))

durationDescriptionParser =
    C.choice
        [ C.regex " weeks?" |> C.map (\_ -> Week)
        , C.regex " days?" |> C.map (\_ -> Day)
        , C.regex " nights?" |> C.map (\_ -> Day)
        ]
