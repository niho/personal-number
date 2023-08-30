module PersonalNumber.UK exposing
    ( NationalInsuranceNumber
    , ValidationError(..)
    , fromString, toString, display
    , decoder, encode
    )

{-| Parse UK National Insurance Numbers.

See <https://www.gov.uk/hmrc-internal-manuals/national-insurance-manual/nim39110> for specification,
and see <https://design-system.service.gov.uk/patterns/national-insurance-numbers/> for designing your UI.

In particular:

  - When asking for a National Insurance Number:
      - allow for 13 characters as National Insurance Numbers are spaced in pairs followed by a single letter
      - let users enter upper and lower case letters, additional spaces and punctuation
      - ignore any unwanted characters before validating
      - avoid using ‘AB 12 34 56 C’ as an example because it belongs to a real person and use ‘QQ 12 34 56 C’ instead
      - set the spellcheck attribute to false so that browsers do not spellcheck the National Insurance Number


# Definition

@docs NationalInsuranceNumber


# Errors

@docs ValidationError


# Strings

@docs fromString, toString, display


# JSON

@docs decoder, encode

-}

import Json.Decode
import Json.Encode


{-| An opaque type representing a valid national insurance number.
-}
type NationalInsuranceNumber
    = NINO String


{-| If the parsing fails an error of this type is returned.

A National Insurance Number is made up of a 2 letter prefix, 6 numbers and a final letter, which is always A, B, C, or D.

It looks something like this:

    QQ 12 34 56 A

All prefixes are valid except:

  - The characters D, F, I, Q, U, and V are not used as either the first or second letter of a prefix.
  - The letter O is not used as the second letter of a prefix.
  - Prefixes BG, GB, KN, NK, NT, TN and ZZ are not to be used

You could turn these errors into strings something like this:

    errorToString : ValidationError -> String
    errorToString e =
        case e of
            InvalidLength ->
                "National Insurance Numbers should have 9 characters, like  QQ 12 34 56 C  or  QQ123456C."

            InvalidFormat DoesNotStartWithTwoLetters ->
                "National Insurance Numbers should start with two letters, like  QQ 12 34 56 C  or  QQ123456C."

            InvalidFormat DoesNotEndWithALetter ->
                "National Insurance Numbers should end with a single letter A, B, C or D, like  QQ 12 34 56 C  or  QQ123456C."

            InvalidFormat DoesNotHaveSixNumbersInTheMiddle ->
                "National Insurance Numbers should have six digits in the middle, like  QQ 12 34 56 C  or  QQ123456C."

            InvalidPrefix DisallowedPrefixLetter ->
                "D, F, I, Q, U, and V are not allowed in the first two letters of National Insurance Numbers."

            InvalidPrefix DisallowedSecondLetterO ->
                "O is not allowed in the second letter of National Insurance Numbers."

            InvalidPrefix DisallowedPrefix ->
                "BG, GB, KN, NK, NT, TN and ZZ are not allowed at the start of National Insurance Numbers."

            InvalidSuffix ->
                "The last letter of National Insurance Numbers can only be A, B, C or D."

-}
type ValidationError
    = InvalidLength -- should be 9 if spaces are removed
    | InvalidFormat FormatError
    | InvalidPrefix PrefixError
    | InvalidSuffix


type FormatError
    = DoesNotStartWithTwoLetters
    | DoesNotHaveSixNumbersInTheMiddle
    | DoesNotEndWithALetter


type PrefixError
    = DisallowedPrefix -- BG, GB, KN, NK, NT, TN, ZZ
    | DisallowedPrefixLetter -- D, F, I, Q, U, V
    | DisallowedSecondLetterO


{-| Converts a national insurance number to string representation in the short format that is commonly used for database storage (AB123456C).
-}
toString : NationalInsuranceNumber -> String
toString (NINO str) =
    str


{-| Format a national insurance number in the long format that is commonly used for human communication (AB 12 34 56 C).
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


{-| Parse a string into a National Insurance Number.
-}
fromString : String -> Result ValidationError NationalInsuranceNumber
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
                |> shouldBe ((==) 9) InvalidLength

        startWithLetters =
            prefix |> shouldBe (String.all Char.isAlpha) (InvalidFormat DoesNotStartWithTwoLetters)

        numbers =
            middleNumber
                |> shouldBe
                    (always <|
                        String.all Char.isDigit middleNumber
                            && String.length middleNumber
                            == 6
                    )
                    (InvalidFormat DoesNotHaveSixNumbersInTheMiddle)

        badPrefixLetter =
            prefix
                |> shouldBe (String.all <| not << inThisList [ 'D', 'F', 'I', 'Q', 'U', 'V' ])
                    (InvalidPrefix DisallowedPrefixLetter)

        badSecondLetter =
            ni
                |> String.slice 1 2
                |> shouldNotBe ((==) "O") (InvalidPrefix DisallowedSecondLetterO)

        disallowedPrefix =
            prefix
                |> shouldNotBe
                    (inThisList [ "BG", "GB", "KN", "NK", "NT", "TN", "ZZ" ])
                    (InvalidPrefix DisallowedPrefix)

        badEnd =
            if not (String.all Char.isAlpha suffix) then
                [ InvalidFormat DoesNotEndWithALetter ]

            else
                suffix
                    |> shouldBe (String.all <| inThisList [ 'A', 'B', 'C', 'D' ])
                        InvalidSuffix

        errors =
            List.concat [ wrongLength, startWithLetters, numbers, badPrefixLetter, badSecondLetter, disallowedPrefix, badEnd ]
    in
    case List.head errors of
        Just error ->
            Err error

        Nothing ->
            Ok (NINO ni)


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
                    Json.Decode.fail "National Insurance Numbers must be in the format QQ 12 34 56 A or QQ123456A. Some letters are not allowed in certain places."
    in
    Json.Decode.string |> Json.Decode.andThen decode


{-| Encode a national insurance number into a JSON value.
-}
encode : NationalInsuranceNumber -> Json.Encode.Value
encode pnr =
    Json.Encode.string (toString pnr)



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
