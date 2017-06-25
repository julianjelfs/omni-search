module OmniSearch exposing (..)

import Date
import Combine as C exposing ((*>), (<*), (>>=), (<|>), (<$>))
import Combine.Num as C
import Combine.Char as C
import Date.Extra.Create as D
import Date.Extra.Core as D

word = C.regex "[A-Za-z]+"

type ProductType
    = Hotel
    | Flight
    | Transfer
    | Holiday

type SearchToken
    = Adults Int
    | Rooms Int
    | ChildAges (List Int)
    | Date Date.Date
    | From String
    | To String
    | Nights Int
    | Product ProductType
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
                            , roomParser
                            , childAgesParser
                            , fromParser
                            , toParser
                            , dateParser
                            , durationParser
                            , productParser
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

wordParser =
    word |> C.map Other

numberOf tagger things =
    C.int <* things
        |> C.map tagger

adultsParser =
    numberOf Adults (C.regex " adults?")

roomParser =
    numberOf Rooms (C.regex " rooms?")

intArrayParser =
    C.sepBy (C.regex " *, *") C.int

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
    (C.string "to " <|> C.string "in " <|> C.string "near ")
        *> word
        |> C.map To

safeStringToInt =
    String.toInt >> Result.withDefault 0

durationParser =
    C.int >>= (\n -> durationDescriptionParser
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

productParser =
    C.choice
        [ (\_ -> Product Hotel) <$> C.regex "hotels?"
        , (\_ -> Product Flight) <$> C.regex "flights?"
        , (\_ -> Product Holiday) <$> C.regex "holiday?"
        , (\_ -> Product Holiday) <$> C.regex "packages?"
        , (\_ -> Product Transfer) <$> C.regex "transfers?"
        ]
