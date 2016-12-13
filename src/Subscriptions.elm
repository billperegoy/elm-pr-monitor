module Subscriptions exposing (..)

import Time
import Model exposing (..)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every Time.second EverySecond
        , Time.every (5 * Time.minute) UpdatePullRequestData
        ]
