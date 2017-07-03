module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import OmniSearch as O
import Date.Extra.Create as D
import Date as D

testDate =
    D.dateFromFields 2017 D.Jul 1 0 0 0 0

firstToken =
    O.parse testDate >> List.head

suite : Test
suite =
    describe "OmniSearch tests"
        [ describe "Adult Parser"
            [ test "Multiple adults" <|
                \() ->
                    case firstToken "12 adults" of
                        Just (O.Adults n) ->
                            Expect.equal n 12
                        _ ->
                            Expect.fail "Incorrect token received"
            , test "Single adult" <|
                \() ->
                    case firstToken "1 adult" of
                        Just (O.Adults n) ->
                            Expect.equal n 1
                        _ ->
                            Expect.fail "Incorrect token received"
            ]
        , describe "Room parser"
            [ test "Multiple rooms" <|
                \() ->
                    case firstToken "3 rooms" of
                        Just (O.Rooms n) ->
                            Expect.equal n 3
                        _ ->
                            Expect.fail "Incorrect token received"
            , test "Single room" <|
                \() ->
                    case firstToken "1 room" of
                        Just (O.Rooms n) ->
                            Expect.equal n 1
                        _ ->
                            Expect.fail "Incorrect token received"
            ]
        , test "Parses childAges correctly" <|
            \() ->
                case firstToken "(1, 4,5 , 6)" of
                    Just (O.ChildAges [1,4,5,6]) ->
                        Expect.pass
                    _ ->
                        Expect.fail "Incorrect token received"
        , test "Parses departure correctly" <|
            \() ->
                case firstToken "from london" of
                    Just (O.From str) ->
                        Expect.equal str "london"
                    _ ->
                        Expect.fail "Incorrect token received"
        , describe "Destination Parser"
            [ test "using 'to'" <|
                \() ->
                    case firstToken "to tenerife" of
                        Just (O.To str) ->
                            Expect.equal str "tenerife"
                        _ ->
                            Expect.fail "Incorrect token received"
            , test "using 'in'" <|
                \() ->
                    case firstToken "in tenerife" of
                        Just (O.To str) ->
                            Expect.equal str "tenerife"
                        _ ->
                            Expect.fail "Incorrect token received"
            , test "using 'near'" <|
                \() ->
                    case firstToken "near tenerife" of
                        Just (O.To str) ->
                            Expect.equal str "tenerife"
                        _ ->
                            Expect.fail "Incorrect token received"
            ]
        , describe "Duration parser"
            [ test "Multiple days" <|
                \() ->
                    case firstToken "10 days" of
                        Just (O.Nights n) ->
                            Expect.equal n 10
                        _ ->
                            Expect.fail "Incorrect token received"
            , test "Single day" <|
                \() ->
                    case firstToken "1 day" of
                        Just (O.Nights n) ->
                            Expect.equal n 1
                        _ ->
                            Expect.fail "Incorrect token received"
            , test "Multiple nights" <|
                \() ->
                    case firstToken "10 nights" of
                        Just (O.Nights n) ->
                            Expect.equal n 10
                        _ ->
                            Expect.fail "Incorrect token received"
            , test "Single night" <|
                \() ->
                    case firstToken "1 night" of
                        Just (O.Nights n) ->
                            Expect.equal n 1
                        _ ->
                            Expect.fail "Incorrect token received"
            , test "Multiple weeks" <|
                \() ->
                    case firstToken "2 weeks" of
                        Just (O.Nights n) ->
                            Expect.equal n 14
                        _ ->
                            Expect.fail "Incorrect token received"
            , test "Single week" <|
                \() ->
                    case firstToken "1 week" of
                        Just (O.Nights n) ->
                            Expect.equal n 7
                        _ ->
                            Expect.fail "Incorrect token received"
            ]
        , describe "Product parser"
            [ test "Hotel" <|
                \() ->
                    case firstToken "hotel" of
                        Just (O.Product O.Hotel) ->
                            Expect.pass
                        _ ->
                            Expect.fail "Incorrect token received"
            , test "Flight" <|
                \() ->
                    case firstToken "flight" of
                        Just (O.Product O.Flight) ->
                            Expect.pass
                        _ ->
                            Expect.fail "Incorrect token received"
            , test "Holiday" <|
                \() ->
                    case firstToken "holiday" of
                        Just (O.Product O.Holiday) ->
                            Expect.pass
                        _ ->
                            Expect.fail "Incorrect token received"
            , test "Package" <|
                \() ->
                    case firstToken "package" of
                        Just (O.Product O.Holiday) ->
                            Expect.pass
                        _ ->
                            Expect.fail "Incorrect token received"
            , test "Transfer" <|
                \() ->
                    case firstToken "transfer" of
                        Just (O.Product O.Transfer) ->
                            Expect.pass
                        _ ->
                            Expect.fail "Incorrect token received"
            , test "CarHire" <|
                \() ->
                    case firstToken "car hire" of
                        Just (O.Product O.CarHire) ->
                            Expect.pass
                        _ ->
                            Expect.fail "Incorrect token received"
            ]
        , describe "Date Parser"
            [ describe "Full Dates"
                [ test "Slashes" <|
                    \() ->
                        case firstToken "10/09/2017" of
                            Just (O.Date d) ->
                                Expect.all
                                    [ \d -> Expect.equal (D.year d) 2017
                                    , \d -> Expect.equal (D.month d) D.Sep
                                    , \d -> Expect.equal (D.day d) 10
                                    ] d

                            _ ->
                                Expect.fail "Incorrect token received"
                , test "Hyphens" <|
                    \() ->
                        case firstToken "10-09-2017" of
                            Just (O.Date d) ->
                                Expect.all
                                    [ \d -> Expect.equal (D.year d) 2017
                                    , \d -> Expect.equal (D.month d) D.Sep
                                    , \d -> Expect.equal (D.day d) 10
                                    ] d

                            _ ->
                                Expect.fail "Incorrect token received"
                ]
            , describe "Relative Dates"
                [ test "tomorrow" <|
                    \() ->
                        case firstToken "tomorrow" of
                            Just (O.Date d) ->
                                Expect.all
                                    [ \d -> Expect.equal (D.year d) 2017
                                    , \d -> Expect.equal (D.month d) D.Jul
                                    , \d -> Expect.equal (D.day d) 2
                                    ] d

                            _ ->
                                Expect.fail "Incorrect token received"]
            , describe "Partial Dates"
                [ test "In the past" <|
                    \() ->
                        case firstToken "1/3" of
                            Just (O.Date d) ->
                                Expect.all
                                    [ \d -> Expect.equal (D.year d) 2018
                                    , \d -> Expect.equal (D.month d) D.Mar
                                    , \d -> Expect.equal (D.day d) 1
                                    ] d

                            _ ->
                                Expect.fail "Incorrect token received"
                , test "In the future" <|
                    \() ->
                        case firstToken "1/8" of
                            Just (O.Date d) ->
                                Expect.all
                                    [ \d -> Expect.equal (D.year d) 2017
                                    , \d -> Expect.equal (D.month d) D.Aug
                                    , \d -> Expect.equal (D.day d) 1
                                    ] d

                            _ ->
                                Expect.fail "Incorrect token received"
                ]
            ]
        , test "Fallback" <|
            \() ->
                let
                    tokens =
                        O.parse testDate "just some randomw words"

                    allOther =
                        tokens
                            |> List.all
                                (\t ->
                                    case t of
                                        O.Other _ -> True
                                        _ -> False
                                )
                in
                    Expect.true "All tokens should be Other type" allOther
        , describe "Combining parsers"
            [ test "Typical search query" <|
                \() ->
                    let
                        tokens =
                            O.parse testDate "hotel in tenerife 2 rooms 2 adults (2,8) 2 weeks 10/08"

                        productFn =
                            tokenExpectation
                                (\t -> case t of
                                    O.Product p -> Just p
                                    _ -> Nothing)
                            O.Hotel

                        destFn =
                            tokenExpectation
                                (\t -> case t of
                                    O.To p -> Just p
                                    _ -> Nothing)
                                "tenerife"

                        roomsFn =
                            tokenExpectation
                                (\t -> case t of
                                    O.Rooms p -> Just p
                                    _ -> Nothing)
                                2

                        adultsFn =
                            tokenExpectation
                                (\t -> case t of
                                    O.Adults p -> Just p
                                    _ -> Nothing)
                                2

                        childAgesFn =
                            tokenExpectation
                                (\t -> case t of
                                    O.ChildAges p -> Just p
                                    _ -> Nothing)
                                [2,8]

                        durationFn =
                            tokenExpectation
                                (\t -> case t of
                                    O.Nights p -> Just p
                                    _ -> Nothing)
                                14
                    in
                        Expect.all
                            [ productFn
                            , destFn
                            , roomsFn
                            , adultsFn
                            , childAgesFn
                            , durationFn
                            ]
                            tokens
            ]
        ]

tokenExpectation filterFn expected =
    (List.filterMap filterFn)
    >> List.head
    >> Expect.equal (Just expected)
