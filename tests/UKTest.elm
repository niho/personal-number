module UKTest exposing (suite)

import Expect
import PersonalNumber.UK as NI
import Test exposing (..)


suite : Test
suite =
    describe "PersonalNumber.UK"
        [ describe "fromString"
            [ test "should accept a valid NI number" <|
                \_ ->
                    NI.fromString "AB123456C"
                        |> Result.map NI.toString
                        |> Expect.equal (Ok "AB123456C")
            , test "should accept a valid NI number with extra spaces" <|
                \_ ->
                    NI.fromString "CB 09 87 65 A"
                        |> Result.map NI.toString
                        |> Expect.equal (Ok "CB098765A")
            , test "should accept a NI number with surrounding whitespace" <|
                \_ ->
                    NI.fromString "  XY 00 99 00 B   "
                        |> Result.map NI.toString
                        |> Expect.equal (Ok "XY009900B")
            , test "should not accept a NINO with just numbers" <|
                \_ -> Expect.err (NI.fromString "12 34 56 78 9")
            , test "should not accept a NINO with just numbers and spaces" <|
                \_ -> Expect.err (NI.fromString "123456789")
            , test "should not accept a NINO with D in the prefix" <|
                \_ -> Expect.err (NI.fromString "DA 12 34 56 A")
            , test "should not accept a NINO with D in the prefix anywhere" <|
                \_ -> Expect.err (NI.fromString "AD 12 34 56 B")
            , test "should not accept a NINO with F in the prefix" <|
                \_ -> Expect.err (NI.fromString "FA 12 34 56 C")
            , test "should not accept a NINO with F in the prefix anywhere" <|
                \_ -> Expect.err (NI.fromString "AF 12 34 56 D")
            , test "should not accept a NINO with I in the prefix" <|
                \_ -> Expect.err (NI.fromString "IA 12 34 56 A")
            , test "should not accept a NINO with I in the prefix anywhere" <|
                \_ -> Expect.err (NI.fromString "AI 12 34 56 B")
            , test "should not accept a NINO with Q in the prefix" <|
                \_ -> Expect.err (NI.fromString "QA 12 34 56 C")
            , test "should not accept a NINO with Q in the prefix anywhere" <|
                \_ -> Expect.err (NI.fromString "AQ 12 34 56 D")
            , test "should not accept a NINO with U in the prefix" <|
                \_ -> Expect.err (NI.fromString "UA 12 34 56 A")
            , test "should not accept a NINO with U in the prefix anywhere" <|
                \_ -> Expect.err (NI.fromString "AU 12 34 56 B")
            , test "should not accept a NINO with V in the prefix" <|
                \_ -> Expect.err (NI.fromString "VA 12 34 56 C")
            , test "should not accept a NINO with V in the prefix anywhere" <|
                \_ -> Expect.err (NI.fromString "AV 12 34 56 D")
            , test "should not accept a NINO with O as the second letter in the prefix" <|
                \_ -> Expect.err (NI.fromString "AO 12 34 56 A")
            , test "should not accept a NINO with the prefix ZZ" <|
                \_ -> Expect.err (NI.fromString "ZZ 00 11 22 B")
            , test "should not accept a NINO with the prefix BG or GB" <|
                \_ -> Expect.err (NI.fromString "BG 33 44 55 C")
            , test "should not accept a NINO with the prefix GB or BG" <|
                \_ -> Expect.err (NI.fromString "GB 55 44 33 D")
            , test "should not accept a NINO with the prefix KN or NK" <|
                \_ -> Expect.err (NI.fromString "KN 66 77 88 A")
            , test "should not accept a NINO with the prefix NK or KN" <|
                \_ -> Expect.err (NI.fromString "NK 88 77 66 B")
            , test "should not accept a NINO with the prefix TN or NT" <|
                \_ -> Expect.err (NI.fromString "TN 99 00 11 C")
            , test "should not accept a NINO with the prefix NT or TN" <|
                \_ -> Expect.err (NI.fromString "NT 11 00 99 D")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not E" <|
                \_ -> Expect.err (NI.fromString "LL000000E")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not F" <|
                \_ -> Expect.err (NI.fromString "LL000000F")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not G" <|
                \_ -> Expect.err (NI.fromString "LL000000G")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not H" <|
                \_ -> Expect.err (NI.fromString "LL000000H")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not I" <|
                \_ -> Expect.err (NI.fromString "LL000000I")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not J" <|
                \_ -> Expect.err (NI.fromString "LL000000J")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not K" <|
                \_ -> Expect.err (NI.fromString "LL000000K")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not L" <|
                \_ -> Expect.err (NI.fromString "LL000000L")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not M" <|
                \_ -> Expect.err (NI.fromString "LL000000M")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not N" <|
                \_ -> Expect.err (NI.fromString "LL000000N")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not O" <|
                \_ -> Expect.err (NI.fromString "LL000000O")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not P" <|
                \_ -> Expect.err (NI.fromString "LL000000P")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not Q" <|
                \_ -> Expect.err (NI.fromString "LL000000Q")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not R" <|
                \_ -> Expect.err (NI.fromString "LL000000R")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not S" <|
                \_ -> Expect.err (NI.fromString "LL000000S")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not T" <|
                \_ -> Expect.err (NI.fromString "LL000000T")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not U" <|
                \_ -> Expect.err (NI.fromString "LL000000U")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not V" <|
                \_ -> Expect.err (NI.fromString "LL000000V")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not W" <|
                \_ -> Expect.err (NI.fromString "LL000000W")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not X" <|
                \_ -> Expect.err (NI.fromString "LL000000X")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not Y" <|
                \_ -> Expect.err (NI.fromString "LL000000Y")
            , test "should not accept a NINO with a final letter that's not A, B, C or D, in particular not Z" <|
                \_ -> Expect.err (NI.fromString "LL000000Z")
            , test "should not accept an empty value" <|
                \_ -> Expect.err (NI.fromString "")
            ]
        , describe "display"
            [ test "should display as a string with spaces" <|
                \_ ->
                    NI.fromString "AB123456C"
                        |> Result.map NI.display
                        |> Expect.equal (Ok "AB 12 34 56 C")
            ]
        , describe "toString"
            [ test "should encode as a string without spaces" <|
                \_ ->
                    NI.fromString "AB 12 34 56 C"
                        |> Result.map NI.toString
                        |> Expect.equal (Ok "AB123456C")
            ]
        ]
