module TimeAgo exposing (timeAgoInWords)

import Time


timeAgoInWords : Time.Time -> String
timeAgoInWords duration =
    if duration <= 30 * Time.second then
        "less than a minute"
    else if duration < 45 * Time.minute then
        reportAsMinutes duration
    else if duration < 24 * Time.hour then
        reportAsHours duration
    else if duration < 30 * 24 * Time.hour then
        reportAsDays duration
    else
        reportAsMonths duration


reportAsMinutes : Time.Time -> String
reportAsMinutes duration =
    let
        minutes =
            round (Time.inMinutes duration)
    in
        if minutes == 1 then
            toString minutes ++ " minute"
        else
            toString minutes ++ " minutes"


reportAsHours : Time.Time -> String
reportAsHours duration =
    let
        hours =
            round (Time.inHours duration)
    in
        if hours == 1 then
            "about an hour"
        else
            "about " ++ toString hours ++ " hours"


reportAsDays : Time.Time -> String
reportAsDays duration =
    let
        days =
            round (Time.inHours duration / 24)
    in
        if days == 1 then
            "1 day"
        else
            toString days ++ " days"


reportAsMonths : Time.Time -> String
reportAsMonths duration =
    let
        months =
            round (Time.inHours duration / (24 * 30))
    in
        if months == 1 then
            "about 1 month"
        else if months <= 59 then
            "about " ++ toString months ++ " months"
        else
            toString months ++ " months"
