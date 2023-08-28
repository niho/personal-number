module PersonalNumber.UK exposing
    ( NationalInsuranceNumber
    , fromString, toString, display
    , ValidationError(..), errorToString, simpleError
    , decoder, encode
    )

{-| Parses UK National Insurance Numbers.

With apologies to the government website for using an example that they asked me NOT to use,

    import PersonalNumber.UK as NI

    NI.fromString "AB123456C" |> Result.map NI.display == Ok "AB 12 34 56 C"

    NI.fromString "AB 12 34 56 C" |> Result.map NI.toString == Ok "AB123456C"

The example they told me to use is deliberately invalid:

    NI.fromString "QQ 12 34 56 C" == Err [ DisallowedPrefixLetter ]

You can get multiple errors

    NI.fromString "GB 123F4567 Z" == [ WrongLength, DoesNotHaveSixNumbersInTheMiddle, DisallowedPrefix, SuffixNotAtoD ]

but if you don't like lists of errors you can simply replace the error with `simpleError`:

    Result.mapError (always NI.simpleError) (NI.fromString "ABC1234567890QQ")
        == OK
            ("National Insurance Numbers should be in the format  QQ 12 34 56 A  or  QQ123456A. "
                ++ " Some letters are not allowed in certain places."
            )

There are encoders and decoders for your JSON codec delight.

See <https://www.gov.uk/hmrc-internal-manuals/national-insurance-manual/nim39110> for specification,
and see <https://design-system.service.gov.uk/patterns/national-insurance-numbers/> for designing your UI.

In particular:

  - When asking for a National Insurance Number:
      - allow for 13 characters as National Insurance Numbers are spaced in pairs followed by a single letter
      - let users enter upper and lower case letters, additional spaces and punctuation
      - ignore any unwanted characters before validating
      - avoid using ‘AB 12 34 56 C’ as an example because it belongs to a real person and use ‘QQ 12 34 56 C’ instead
      - set the spellcheck attribute to false so that browsers do not spellcheck the National Insurance Number

I've done all but the last one for you in `fromString`.


# Definition

@docs NationalInsuranceNumber


# Strings

@docs fromString, toString, display


# Errors

@docs ValidationError, errorToString, simpleError


# JSON

@docs decoder, encode

-}

import Json.Decode
import Json.Encode


{-| An opaque type representing a valid personal number.
-}
type NationalInsuranceNumber
    = NINO String


{-| If the parsing fails an error of this type is returned.
From <https://www.gov.uk/hmrc-internal-manuals/national-insurance-manual/nim39110>

A National Insurance Number is made up of 2 letters, 6 numbers and a final letter, which is always A, B, C, or D.

It looks something like this:

    QQ 12 34 56 A

This is an example only and should not be used as an actual number.
All prefixes are valid except:

  - The characters D, F, I, Q, U, and V are not used as either the first or second letter of a NINO prefix.
  - The letter O is not used as the second letter of a prefix.
  - Prefixes BG, GB, KN, NK, NT, TN and ZZ are not to be used

-}
type ValidationError
    = WrongLength -- should be 9 if spaces are removed
    | DoesNotStartWithTwoLetters
    | DoesNotHaveSixNumbersInTheMiddle
    | DoesNotEndWithALetter
    | DisallowedPrefix -- BG, GB, KN, NK, NT, TN, ZZ
    | DisallowedPrefixLetter -- D, F, I, Q, U, V
    | DisallowedSecondLetterO
    | SuffixNotAtoD


{-| Describing errors in a String.

    List.map NI.errorToString
        [ NI.DoesNotStartWithTwoLetters
        , NI.DoesNotHaveSixNumbersInTheMiddle
        , NI.DoesNotEndWithALetter
        , NI.DisallowedPrefix
        , NI.DisallowedPrefixLetter
        , NI.DisallowedSecondLetterO
        , NI.SuffixNotAtoD
        ]
        [ "National Insurance Numbers should start with two letters, like  QQ 12 34 56 C  or  QQ123456C."
        , "National Insurance Numbers should have six digits in the middle, like QQ 12 34 56 C or QQ123456C."
        , "National Insurance Numbers should end with a single letter A, B, C or D, like QQ 12 34 56 C or QQ123456C."
        , "BG, GB, KN, NK, NT, TN and ZZ are not allowed at the start of National Insurance Numbers."
        , "D, F, I, Q, U, and V are not allowed in the first two letters of National Insurance Numbers."
        , "O is not allowed in the second letter of National Insurance Numbers."
        , "The last letter of National Insurance Numbers can only be A, B, C or D."
        ]

-}
errorToString : ValidationError -> String
errorToString e =
    case e of
        WrongLength ->
            "National Insurance Numbers should have 9 characters, like  QQ 12 34 56 C  or  QQ123456C."

        DoesNotStartWithTwoLetters ->
            "National Insurance Numbers should start with two letters, like  QQ 12 34 56 C  or  QQ123456C."

        DoesNotEndWithALetter ->
            "National Insurance Numbers should end with a single letter A, B, C or D, like  QQ 12 34 56 C  or  QQ123456C."

        DoesNotHaveSixNumbersInTheMiddle ->
            "National Insurance Numbers should have six digits in the middle, like  QQ 12 34 56 C  or  QQ123456C."

        DisallowedPrefixLetter ->
            "D, F, I, Q, U, and V are not allowed in the first two letters of National Insurance Numbers."

        DisallowedSecondLetterO ->
            "O is not allowed in the second letter of National Insurance Numbers."

        DisallowedPrefix ->
            "BG, GB, KN, NK, NT, TN and ZZ are not allowed at the start of National Insurance Numbers."

        SuffixNotAtoD ->
            "The last letter of National Insurance Numbers can only be A, B, C or D."


{-| Converts a personal number to string representation in the short format that is commonly used for database storage (AB123456C).

    NI.fromString "AB 12 34 56 C"
        |> Result.map NI.toString
        == Ok "AB123456C"

    NI.fromString "AB123456C"
        |> Result.map NI.toString
        == Ok "AB123456C"

-}
toString : NationalInsuranceNumber -> String
toString (NINO str) =
    str


{-| Formats a personal number to string representation in the long format that is commonly used for human communication (AB 12 34 56 C).

    NI.fromString "AB 12 34 56 C"
        |> Result.map NI.display
        == Ok "AB 12 34 56 C"

    NI.fromString "AB123456C"
        |> Result.map NI.display
        == Ok "AB 12 34 56 C"

-}
display : NationalInsuranceNumber -> String
display (NINO str) =
    String.join " "
        [ String.slice 0 2 str
        , String.slice 2 4 str
        , String.slice 4 6 str
        , String.slice 6 8 str
        , String.slice 8 9 str
        ]



-- Helpers


shouldBe : (a -> Bool) -> error -> a -> List error
shouldBe test error a =
    if test a then
        []

    else
        [ error ]


shouldNotBe : (a -> Bool) -> error -> a -> List error
shouldNotBe test error a =
    shouldBe (test >> not) error a


inThisList : List a -> a -> Bool
inThisList list a =
    List.member a list


{-| Parse a string into a National Insurance Number, listing all errors if there are any.
It removes superfluous spaces and any punctuation, as the government recommends.

    import PersonalNumber.UK as NI

    NI.fromString "tt_12-34-56   b   "
        |> Result.map NI.display
        == Ok "TT 12 34 56 B"

    NI.fromString "12 1234567 9"
        == Err [ WrongLength, DoesNotStartWithTwoLetters, DoesNotHaveSixNumbersInTheMiddle, DoesNotEndWithALetter ]

    NI.fromString "GB 12 AB 56 E"
        == Err [ DoesNotHaveSixNumbersInTheMiddle, DisallowedPrefix, SuffixNotAtoD ]

    NI.fromString "IO 00 00 00 A"
        == Err [ DisallowedPrefixLetter, DisallowedSecondLetterO ]

-}
fromString : String -> Result (List ValidationError) NationalInsuranceNumber
fromString str =
    let
        ni =
            -- trim and remove non alpha-numeric content and capitalise to be as forgiving as possible
            str
                |> String.filter Char.isAlphaNum
                |> String.toUpper

        prefix =
            String.left 2 ni

        middleNumber =
            String.slice 2 -1 ni

        suffix =
            String.right 1 ni

        wrongLength =
            ni
                |> String.length
                |> shouldBe ((==) 9) WrongLength

        startWithLetters =
            prefix |> shouldBe (String.all Char.isAlpha) DoesNotStartWithTwoLetters

        numbers =
            middleNumber
                |> shouldBe
                    (always <|
                        String.all Char.isDigit middleNumber
                            && String.length middleNumber
                            == 6
                    )
                    DoesNotHaveSixNumbersInTheMiddle

        badPrefixLetter =
            prefix
                |> shouldBe (String.all <| not << inThisList [ 'D', 'F', 'I', 'Q', 'U', 'V' ])
                    DisallowedPrefixLetter

        badSecondLetter =
            ni
                |> String.slice 1 2
                |> shouldNotBe ((==) "O") DisallowedSecondLetterO

        disallowedPrefix =
            prefix
                |> shouldNotBe
                    (inThisList [ "BG", "GB", "KN", "NK", "NT", "TN", "ZZ" ])
                    DisallowedPrefix

        badEnd =
            if not (String.all Char.isAlpha suffix) then
                [ DoesNotEndWithALetter ]

            else
                suffix
                    |> shouldBe (String.all <| inThisList [ 'A', 'B', 'C', 'D' ])
                        SuffixNotAtoD

        errors =
            List.concat [ wrongLength, startWithLetters, numbers, badPrefixLetter, badSecondLetter, disallowedPrefix, badEnd ]
    in
    if List.isEmpty errors then
        Ok (NINO ni)

    else
        Err errors


{-| Provide a simple catch-all error for invalid NI Numbers

    simpleError
        == "National Insurance Numbers should be in the format  QQ 12 34 56 A  or  QQ123456A. "
        ++ " Some letters are not allowed in certain places."

-}
simpleError : String
simpleError =
    "National Insurance Numbers should be in the format  QQ 12 34 56 A  or  QQ123456A.  Some letters are not allowed in certain places."


{-| Decode a National Insurance Number.
-}
decoder : Json.Decode.Decoder NationalInsuranceNumber
decoder =
    let
        decode str =
            case fromString str of
                Ok pnr ->
                    Json.Decode.succeed pnr

                Err _ ->
                    Json.Decode.fail simpleError
    in
    Json.Decode.string |> Json.Decode.andThen decode


{-| Encode a personal number into a JSON value.
-}
encode : NationalInsuranceNumber -> Json.Encode.Value
encode pnr =
    Json.Encode.string (toString pnr)
