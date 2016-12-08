module DateTimeUtils exposing (..)

import Time
import Date
import String


dateStringToTime : String -> Time.Time
dateStringToTime dateString =
    let
        dateResult =
            Date.fromString dateString
    in
        case dateResult of
            Ok value ->
                Date.toTime value

            Err _ ->
                0.0


timeStringToFloat : String -> Float -> Float
timeStringToFloat string default =
    let
        convertedValue =
            String.toFloat string
    in
        Result.withDefault default convertedValue
