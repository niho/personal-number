module NorwegianTest exposing (suite)

import Expect
import PersonalNumber.Norwegian as PersonalNumber exposing (fromString)
import Test exposing (..)


suite : Test
suite =
    describe "PersonalNumber.Norwegian"
        [ describe "fromString"
            [ test "should accept a valid PNR" <|
                \_ ->
                    fromString "31013626696"
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "31013626696")
            , test "should accept a PNR with D-number format" <|
                \_ ->
                    fromString "51111199993"
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "51111199993")
            , test "should accept a PNR with H-number format" <|
                \_ ->
                    fromString "02417544944"
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "02417544944")
            , test "should accept a PNR with surrounding whitespace" <|
                \_ ->
                    fromString "  19118438739  "
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "19118438739")
            , test "should not accept a PNR without the last four digits" <|
                \_ -> Expect.err (fromString "200866001386")
            , test "should not accept an invalid PNR with the correct format" <|
                \_ -> Expect.err (fromString "  - - -- ")
            , test "should not accept a PNR that is not a valid date" <|
                \_ -> Expect.err (fromString "1407680023")
            , test "should not accept a PNR with an invalid checksum" <|
                \_ -> Expect.err (fromString "29029900158")
            , test "should not accept an empty value" <|
                \_ -> Expect.err (fromString "")
            ]
        , describe "display"
            [ test "should encode as a string with dash" <|
                \_ ->
                    fromString "19118438739"
                        |> Result.map PersonalNumber.display
                        |> Expect.equal (Ok "191184-387-39")
            ]
        , describe "toString"
            [ test "should encode as a string without dash" <|
                \_ ->
                    fromString "31013626696"
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "31013626696")
            ]
        ]
