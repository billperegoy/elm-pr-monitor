module DateUtils exposing (..)

import Time
import Date


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
