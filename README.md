# Personal number

A library for parsing personal numbers. The library is designed to be forgiving on input, but strict on output. It also has useful errors and JSON decoders/encoders.

## Support

- Swedish personal numbers (personnummer).
- Swedish personal numbers for foreign visitors (sammordningsnummer).
- Norwegian personal numbers (fødselsnummer).

## Example usage

```elm
import PersonalNumber.Swedish as PersonalNumber exposing (ValidationError(..))


personalNumber : String -> String
personalNumber str =
  case PersonalNumber.fromString str of
    Ok pnr ->
      -- The `pnr` is wrapped in a `PersonalNumber` type and is
      -- guaranteed to be valid. Use the `display` function to turn
      -- it back into a user readable string.
      PersonalNumber.display pnr

    Err InvalidFormat ->
      "Not the correct format for a swedish personal number."

    Err InvalidLength ->
      "The personal number is not of the correct length."

    Err InvalidDate ->
      "The first part of the personal number needs to be a valid date."

    Err InvalidChecksum ->
      "One or more digits in the personal number is wrong."
```

## JSON

```elm
import PersonalNumber.Swedish as PersonalNumber
import Json.Decode exposing (Decoder, map2, field)
import Json.Encode exposing (Value, object)

type alias Person =
  { personalNumber : PersonalNumber.PersonalNumber
  }

decoder : Decoder Person
decoder =
  map2 Person
    (field "personal_number" PersonalNumber.decoder)


encode : Person -> Value
encode person =
  object
    [ ( "personal_number", PersonalNumber.encode person.personalNumber )
    ]
```
