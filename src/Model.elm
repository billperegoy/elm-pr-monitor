module Model exposing (..)

import Dict
import Time
import ModelUpdate
import Github
import Http
import Config


type alias Model =
    { currentTime : Time.Time
    , pullRequests : ModelUpdate.PullRequestCollection
    , decayTimeFormValue : String
    , decayTimeInDays : Float
    , errors : Maybe String
    }



--
-- Init
--


initModel : Model
initModel =
    { currentTime = 0.0
    , pullRequests = Dict.empty
    , decayTimeFormValue = ""
    , decayTimeInDays = 5
    , errors = Nothing
    }



--
-- Update
--


type Msg
    = GetPullRequestData (Result Http.Error (List Github.PullRequestData))
    | GetPullRequestCommentData (Result Http.Error (List Github.PullRequestCommentData))
    | GetPullRequestIssuesData (Result Http.Error Github.IssuesData)
    | GetPullRequestStatusData (Result Http.Error (List Github.StatusData))
    | SetDecayTimeFormValue String
    | UpdateDecayTime
    | EverySecond Float
    | UpdatePullRequestData Float
