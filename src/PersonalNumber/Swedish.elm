module PersonalNumber.Swedish exposing
    ( PersonalNumber
    , ValidationError(..)
    , decoder
    , display
    , encode
    , fromString
    , toString
    )

import Json.Decode
import Json.Encode
import Regex
import String exposing (filter, join, length, slice, startsWith, trim)


type PersonalNumber
    = PNR String
    | SAM String


type ValidationError
    = InvalidFormat
    | InvalidLength
    | InvalidDate
    | InvalidChecksum


format : Regex.Regex
format =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^(19|20)[0-9]{6}[0-9]{4}$"


checkFormat : String -> Result ValidationError String
checkFormat str =
    case Regex.contains format str of
        False ->
            Err InvalidFormat

        True ->
            Ok str


verifyChecksum : String -> Result ValidationError String
verifyChecksum str =
    let
        regex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "\\D"

        checksum =
            str
                |> Regex.replace regex (\_ -> "")
                |> String.split ""
                |> List.reverse
                |> List.take 10
                |> List.reverse
                |> List.map String.toInt
                |> List.map (Maybe.withDefault -1)
                |> List.indexedMap (\a b -> ( a, b ))
                |> List.map
                    (\( i, a ) ->
                        if modBy 2 i == 0 then
                            a * 2

                        else
                            a
                    )
                |> List.map
                    (\a ->
                        if a > 9 then
                            1 + (a - 10)

                        else
                            a
                    )
                |> List.foldl (+) 0
    in
    case modBy 10 checksum == 0 of
        True ->
            Ok str

        False ->
            Err InvalidChecksum


numberType : String -> Result ValidationError PersonalNumber
numberType str =
    let
        year =
            slice 0 4 str |> String.toInt |> Maybe.withDefault 0

        month =
            slice 4 6 str |> String.toInt |> Maybe.withDefault 0

        day =
            slice 6 8 str |> String.toInt |> Maybe.withDefault 0
    in
    if year >= 1900 && month >= 1 && month <= 12 then
        if day >= 1 && day <= 31 then
            Ok (PNR str)

        else if day >= 61 && day <= 91 then
            Ok (SAM str)

        else
            Err InvalidDate

    else
        Err InvalidDate


fromString : String -> Result ValidationError PersonalNumber
fromString str =
    let
        pnr =
            trim <| filter (\c -> c /= '-') str
    in
    case length pnr of
        10 ->
            if
                startsWith "0" pnr
                    || startsWith "1" pnr
            then
                fromString ("20" ++ pnr)

            else
                fromString ("19" ++ pnr)

        12 ->
            let
                date =
                    String.left 8 pnr

                digits =
                    String.right 4 pnr
            in
            checkFormat (date ++ digits)
                |> Result.andThen verifyChecksum
                |> Result.andThen numberType

        _ ->
            Err InvalidLength


toString : PersonalNumber -> String
toString pnr =
    case pnr of
        PNR str ->
            str

        SAM str ->
            str


encode : PersonalNumber -> Json.Encode.Value
encode pnr =
    Json.Encode.string (toString pnr)


decoder : Json.Decode.Decoder PersonalNumber
decoder =
    let
        decode str =
            case fromString str of
                Ok pnr ->
                    Json.Decode.succeed pnr

                Err err ->
                    Json.Decode.fail "Invalid personal number."
    in
    Json.Decode.string |> Json.Decode.andThen decode


display : PersonalNumber -> String
display pnr =
    let
        str =
            toString pnr

        date =
            String.left 8 str

        digits =
            String.right 4 str
    in
    join "-" [ date, digits ]
