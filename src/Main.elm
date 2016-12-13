module Main exposing (..)

import Html exposing (program)
import Time
import Model
import Update
import View
import Config
import Subscriptions


main : Program Never Model.Model Model.Msg
main =
    Html.program
        { init = Model.initModel ! Update.getAllPullRequestData Config.repositories
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }



--
-- Subscriptions
--


subscriptions : Model.Model -> Sub Model.Msg
subscriptions _ =
    Sub.batch
        [ Time.every Time.second Model.EverySecond
        , Time.every (5 * Time.minute) Model.UpdatePullRequestData
        ]
