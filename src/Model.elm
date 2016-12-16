module Model exposing (..)

import Dict
import Http
import Time


type alias Model =
    { currentTime : Time.Time
    , pullRequests : PullRequestCollection
    , errors : Maybe String
    , decayTimeInDays : Float
    }


type alias PullRequestCollection =
    Dict.Dict String AugmentedPullRequestData


type alias PullRequestData =
    { number : Int
    , htmlUrl : String
    , commentsUrl : String
    , issueUrl : String
    , statusesUrl : String
    , title : String
    , body : String
    , state : String
    , createdAt : String
    , head : GitElementData
    , base : GitElementData
    , user : UserData
    }


type alias AugmentedPullRequestData =
    { number : Int
    , htmlUrl : String
    , commentsUrl : String
    , issueUrl : String
    , statusesUrl : String
    , title : String
    , body : String
    , state : String
    , createdAt : String
    , head : GitElementData
    , base : GitElementData
    , user : UserData
    , comments : List PullRequestCommentData
    , labels : List PullRequestLabel
    , buildStatus : BuildStatus
    }


type alias PullRequestCommentData =
    { body : String
    , user : UserData
    , issueUrl : String
    }


type alias IssuesData =
    { url : String
    , number : Int
    , labels : List PullRequestLabel
    }


type alias StatusData =
    { url : String
    , description : String
    , state : String
    }


type alias GitElementData =
    { repo : RepoData }


type alias RepoData =
    { name : String
    , fullName : String
    }


type alias UserData =
    { login : String
    }


type alias PullRequestLabel =
    { name : String
    , color : String
    }


type BuildStatus
    = Pending
    | Success
    | Fail


initModel : Model
initModel =
    { currentTime = 0.0
    , pullRequests = Dict.empty
    , errors = Nothing
    , decayTimeInDays = 5
    }


type Msg
    = GetPullRequestData (Result Http.Error (List PullRequestData))
    | GetPullRequestCommentData (Result Http.Error (List PullRequestCommentData))
    | GetPullRequestIssuesData (Result Http.Error IssuesData)
    | GetPullRequestStatusData (Result Http.Error (List StatusData))
    | EverySecond Float
    | UpdatePullRequestData Float
