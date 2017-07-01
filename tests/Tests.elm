module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import OmniSearch as O
import Date.Extra.Create as D
import Date as D

testDate =
    D.dateFromFields 2017 D.Jul 1 0 0 0 0

suite : Test
suite =
    describe "OmniSearch tests"
        [ test "Parses number of adults correctly" <|
            \() ->
                let
                    token =
                        O.parse testDate "122 adults"
                            |> List.head
                in
                    case token of
                        Nothing ->
                            Expect.fail "No tokens received"
                        Just (O.Adults n) ->
                            Expect.equal n 12
                        _ ->
                            Expect.fail "Incorrect token received"
        ]
