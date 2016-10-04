module TimeAgo exposing (timeAgoInWords)

import Time exposing (..)


timeAgoInWords : Time -> String
timeAgoInWords duration =
    if duration <= 30 * second then
        "less than a minute"
    else if duration < 45 * minute then
        reportAsMinutes duration
    else if duration < 24 * hour then
        reportAsHours duration
    else if duration < 30 * 24 * hour then
        reportAsDays duration
    else
        reportAsMonths duration


reportAsMinutes : Time -> String
reportAsMinutes duration =
    let
        minutes =
            round (inMinutes duration)
    in
        if minutes == 1 then
            toString minutes ++ " minute"
        else
            toString minutes ++ " minutes"


reportAsHours : Time -> String
reportAsHours duration =
    let
        hours =
            round (inHours duration)
    in
        if hours == 1 then
            "about an hour"
        else
            "about " ++ toString hours ++ " hours"


reportAsDays : Time -> String
reportAsDays duration =
    let
        days =
            round (inHours duration / 24)
    in
        if days == 1 then
            "1 day"
        else
            toString days ++ " days"


reportAsMonths : Time -> String
reportAsMonths duration =
    let
        months =
            round (inHours duration / (24 * 30))
    in
        if months == 1 then
            "about 1 month"
        else if months <= 59 then
            "about " ++ toString months ++ " months"
        else
            toString months ++ " months"
