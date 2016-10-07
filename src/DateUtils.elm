module DateUtils exposing (..)

import Time exposing (..)
import Date exposing (..)


dateStringToTime : String -> Time
dateStringToTime dateString =
    let
        dateResult =
            Date.fromString dateString
    in
        case dateResult of
            Ok value ->
                toTime value

            Err _ ->
                0.0
