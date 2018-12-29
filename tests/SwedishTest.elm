module SwedishTest exposing (suite)

import Expect exposing (Expectation)
import PersonalNumber.Swedish as PersonalNumber
    exposing
        ( PersonalNumber(..)
        , fromString
        )
import Test exposing (..)


suite : Test
suite =
    describe "PersonalNumber.Swedish"
        [ describe "fromString"
            [ test "should accept a valid PNR" <|
                \_ ->
                    fromString "19921208-1286"
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "199212081286")
            , test "should accept a PNR with a two-digit year format" <|
                \_ ->
                    fromString "921208-1286"
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "199212081286")
            , test "should accept a PNR without a dash" <|
                \_ ->
                    fromString "199212081286"
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "199212081286")
            , test "should accept a two-digit year PNR without a dash" <|
                \_ ->
                    fromString "9212081286"
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "199212081286")
            , test "should accept a PNR with surrounding whitespace" <|
                \_ ->
                    fromString "  19921208-1286  "
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "199212081286")
            , test "should not accept a PNR without the last four digits" <|
                \_ -> Expect.err (fromString "19921208")
            , test "should not accept an invalid PNR with the correct format" <|
                \_ -> Expect.err (fromString "00000000-0000")
            , test "should not accept a PNR that is not a valid date" <|
                \_ -> Expect.err (fromString "19921308-1285")
            , test "should not accept a PNR with an invalid checksum" <|
                \_ -> Expect.err (fromString "19921208-1280")
            , test "should not accept an empty value" <|
                \_ -> Expect.err (fromString "")
            , test "should accept a valid SAM (sammordningsnummer)" <|
                \_ ->
                    fromString "701063-1237"
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "197010631237")
            ]
        , describe "display"
            [ test "should encode as a string with dash" <|
                \_ ->
                    fromString "19921208-1286"
                        |> Result.map PersonalNumber.display
                        |> Expect.equal (Ok "19921208-1286")
            ]
        , describe "toString"
            [ test "should encode as a string without dash" <|
                \_ ->
                    fromString "19921208-1286"
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "199212081286")
            ]
        ]
