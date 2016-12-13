module Model exposing (..)

import Dict
import Time
import Github
import Http


type alias Model =
    { currentTime : Time.Time
    , pullRequests : Github.PullRequestCollection
    , decayTimeFormValue : String
    , decayTimeInDays : Float
    , errors : Maybe String
    }


initModel : Model
initModel =
    { currentTime = 0.0
    , pullRequests = Dict.empty
    , decayTimeFormValue = ""
    , decayTimeInDays = 5
    , errors = Nothing
    }


type Msg
    = GetPullRequestData (Result Http.Error (List Github.PullRequestData))
    | GetPullRequestCommentData (Result Http.Error (List Github.PullRequestCommentData))
    | GetPullRequestIssuesData (Result Http.Error Github.IssuesData)
    | GetPullRequestStatusData (Result Http.Error (List Github.StatusData))
    | SetDecayTimeFormValue String
    | UpdateDecayTime
    | EverySecond Float
    | UpdatePullRequestData Float
