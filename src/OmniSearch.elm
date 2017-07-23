module OmniSearch exposing (parse, SearchToken(..), ProductType(..))

import Date exposing (Date, year, month, day)
import Combine as C exposing ((*>), (<*), (>>=), (<|>), (<$>))
import Combine.Num as C
import Combine.Char as C
import Date.Extra.Create as D
import Date.Extra.Core as D
import Date.Extra.Period as D
import Date.Extra.Compare as D
import String exposing (join, lines)

word = C.regex "[A-Za-z0-9]+"

type ProductType
    = Hotel
    | Flight
    | Transfer
    | Holiday
    | CarHire

type ChildAge
    = ChildAge Int
    | UnknownAge

type SearchToken
    = Adults Int
    | Rooms Int
    | ChildAges (List ChildAge)
    | Date Date.Date
    | From String
    | To String
    | Nights Int
    | Product ProductType
    | Other String

type DurationType
    = Week
    | Day

flatten : String -> String
flatten =
    lines >> join " "

parse : Date -> String -> List SearchToken
parse now txt =
    parseInternal now (flatten txt) []


parseInternal : Date -> String -> List SearchToken -> List SearchToken
parseInternal now txt tokens =
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
                            , (dateParser now)
                            , (relativeDateParser now)
                            , durationParser
                            , productParser
                            , wordParser
                            ])
            in
                case res of
                    Ok (_, { input }, result) ->
                        parseInternal now input (result :: tokens)
                    _ ->
                        case C.parse C.space txt of
                            Ok (_, { input }, result) ->
                                parseInternal now input tokens
                            _ -> tokens

wordParser =
    word |> C.map Other

numberOf tagger things =
    C.int <* things
        |> C.map tagger

adultsParser =
    numberOf Adults ((C.regex " adults?") <|> (C.regex " people") <|> (C.regex " person"))

roomParser =
    numberOf Rooms (C.regex " rooms?")

intArrayParser =
    C.sepBy (C.regex " *, *") C.int


childAgesParser =
    C.regex "\\( *"
        *> intArrayParser
        <* C.regex " *\\)"
        |> C.map (List.map ChildAge)
        |> C.map ChildAges

relativeDateParser now =
    tomorrowParser now

tomorrowParser now =
    C.string "tomorrow"
        |> C.map (\_ ->
            let
                t = D.add D.Day 1 now
            in
                Date <| D.dateFromFields (year t) (month t) (day t) 0 0 0 0)

dateParser now =
    clamp 1 31 <$> datePartParser
        >>= (\d -> clamp 1 12 <$> datePartParser
        >>= (\m ->
            C.optional (year now)
                datePartParser
                    |> C.map (\y ->
                        let
                            date = D.dateFromFields y (D.intToMonth m) d 0 0 0 0
                        in
                            case D.is D.Before date now of
                                True -> Date <| D.dateFromFields (y+1) (D.intToMonth m) d 0 0 0 0
                                False -> Date date)))

datePartParser =
    C.regex "[0-9]+" <* (C.maybe (C.regex "(-|/)"))
        |> C.map safeStringToInt

fromParser =
    C.string "from " *> placeParser
        |> C.map From

inQuotes =
    (C.between (C.string "\"") (C.string "\"") (C.regex "[a-zA-Z 0-9]*"))

placeParser =
    word <|> inQuotes

toParser =
    (C.string "to " <|> C.string "in " <|> C.string "near ")
        *> placeParser
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
        , (\_ -> Product CarHire) <$> C.regex "car hire"
        , (\_ -> Product CarHire) <$> C.regex "carhire"
        ]
