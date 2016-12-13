module Main exposing (..)

import Html exposing (program)


--

import Model
import Subscriptions
import Update
import View


main : Program Never Model.Model Model.Msg
main =
    Html.program
        { init =
            ( Model.initModel
            , Cmd.batch (Update.getAllPullRequestData)
            )
        , view = View.view
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }
