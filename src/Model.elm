module Model exposing (..)

import Dict
import Time
import Github
import Http


type alias Model =
    { currentTime : Time.Time
    , pullRequests : PullRequestCollection
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


type alias PullRequestCollection =
    Dict.Dict String Github.AugmentedPullRequestData


pullRequestKey : Github.AugmentedPullRequestData -> String
pullRequestKey pullRequest =
    pullRequest.base.repo.fullName ++ ":" ++ toString pullRequest.number


pullRequestListToDict : List Github.AugmentedPullRequestData -> PullRequestCollection
pullRequestListToDict pullRequests =
    let
        zippedList =
            List.map (\e -> ( pullRequestKey e, e )) pullRequests
    in
        Dict.fromList zippedList


issueUrlToDictKey : String -> String
issueUrlToDictKey url =
    Github.issueUrlToRepository
        url
        ++ ":"
        ++ Github.issueUrlToPullRequestId url


addLabels : PullRequestCollection -> Github.IssuesData -> PullRequestCollection
addLabels pullRequests issue =
    let
        key =
            issueUrlToDictKey issue.url

        pr : Maybe Github.AugmentedPullRequestData
        pr =
            Dict.get key pullRequests

        newPr =
            Maybe.map (\pr -> { pr | labels = issue.labels }) pr
    in
        case newPr of
            Nothing ->
                pullRequests

            Just a ->
                Dict.union (Dict.singleton key a) pullRequests


addComments : PullRequestCollection -> List Github.PullRequestCommentData -> PullRequestCollection
addComments pullRequests comments =
    let
        key : String
        key =
            comments
                |> List.map (\e -> issueUrlToDictKey e.issueUrl)
                |> List.head
                |> Maybe.withDefault "error"

        pr : Maybe Github.AugmentedPullRequestData
        pr =
            Dict.get key pullRequests

        extractComments : Github.AugmentedPullRequestData -> List Github.PullRequestCommentData -> Github.AugmentedPullRequestData
        extractComments pr comments =
            { pr
                | comments =
                    List.filter
                        (\e ->
                            (String.contains "👍" e.body)
                                || (String.contains ":+1" e.body)
                        )
                        comments
            }

        newPr : Maybe Github.AugmentedPullRequestData
        newPr =
            Maybe.map (\pull -> extractComments pull comments) pr
    in
        case newPr of
            Nothing ->
                pullRequests

            Just a ->
                Dict.union (Dict.singleton key a) pullRequests


updatePullRequests : PullRequestCollection -> List Github.PullRequestData -> PullRequestCollection
updatePullRequests pullRequests newPullRequests =
    Dict.union
        (newPullRequests
            |> List.map (\e -> Github.augmentPullRequestData e)
            |> pullRequestListToDict
        )
        pullRequests
