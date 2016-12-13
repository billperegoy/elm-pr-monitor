module Github exposing (..)

import Http
import Json.Decode
import Json.Decode.Pipeline
import String
import DateTimeUtils
import Dict
import Model


augmentPullRequestData : Model.PullRequestData -> Model.AugmentedPullRequestData
augmentPullRequestData elem =
    { number = elem.number
    , htmlUrl = elem.htmlUrl
    , commentsUrl = elem.commentsUrl
    , issueUrl = elem.issueUrl
    , statusesUrl = elem.statusesUrl
    , title = elem.title
    , body = elem.body
    , state = elem.state
    , createdAt = elem.createdAt
    , head = elem.head
    , base = elem.base
    , user = elem.user
    , comments = []
    , labels = []
    , buildStatus = Model.Fail
    }


issuesDecoder : Json.Decode.Decoder Model.IssuesData
issuesDecoder =
    Json.Decode.Pipeline.decode Model.IssuesData
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "number" Json.Decode.int
        |> Json.Decode.Pipeline.required "labels" pullRequestLabelListDecoder


statusListDecoder : Json.Decode.Decoder (List Model.StatusData)
statusListDecoder =
    Json.Decode.list statusDecoder


statusDecoder : Json.Decode.Decoder Model.StatusData
statusDecoder =
    Json.Decode.Pipeline.decode Model.StatusData
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "state" Json.Decode.string


pullRequestLabelListDecoder : Json.Decode.Decoder (List Model.PullRequestLabel)
pullRequestLabelListDecoder =
    Json.Decode.list pullRequestLabelDecoder


pullRequestLabelDecoder : Json.Decode.Decoder Model.PullRequestLabel
pullRequestLabelDecoder =
    Json.Decode.Pipeline.decode Model.PullRequestLabel
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "color" Json.Decode.string


pullRequestCommentListDecoder : Json.Decode.Decoder (List Model.PullRequestCommentData)
pullRequestCommentListDecoder =
    Json.Decode.list pullRequestCommentDecoder


pullRequestCommentDecoder : Json.Decode.Decoder Model.PullRequestCommentData
pullRequestCommentDecoder =
    Json.Decode.Pipeline.decode Model.PullRequestCommentData
        |> Json.Decode.Pipeline.required "body" Json.Decode.string
        |> Json.Decode.Pipeline.required "user" userDecoder
        |> Json.Decode.Pipeline.required "issue_url" Json.Decode.string


pullRequestListDecoder : Json.Decode.Decoder (List Model.PullRequestData)
pullRequestListDecoder =
    Json.Decode.list pullRequestDataDecoder


pullRequestDataDecoder : Json.Decode.Decoder Model.PullRequestData
pullRequestDataDecoder =
    Json.Decode.Pipeline.decode Model.PullRequestData
        |> Json.Decode.Pipeline.required "number" Json.Decode.int
        |> Json.Decode.Pipeline.required "html_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "comments_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "issue_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "statuses_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "body" Json.Decode.string
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "head" gitElementDecoder
        |> Json.Decode.Pipeline.required "base" gitElementDecoder
        |> Json.Decode.Pipeline.required "user" userDecoder


gitElementDecoder : Json.Decode.Decoder Model.GitElementData
gitElementDecoder =
    Json.Decode.Pipeline.decode Model.GitElementData
        |> Json.Decode.Pipeline.required "repo" repoDecoder


userDecoder : Json.Decode.Decoder Model.UserData
userDecoder =
    Json.Decode.Pipeline.decode Model.UserData
        |> Json.Decode.Pipeline.required "login" Json.Decode.string


repoDecoder : Json.Decode.Decoder Model.RepoData
repoDecoder =
    Json.Decode.Pipeline.decode Model.RepoData
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "full_name" Json.Decode.string


issueUrlToRepository url =
    String.split "/" url
        |> List.drop 6
        |> List.take 2
        |> String.join "/"


issueUrlToPullRequestId : String -> String
issueUrlToPullRequestId url =
    String.split "/" url
        |> List.drop 9
        |> List.head
        |> Maybe.withDefault "error"


pullRequestKey : Model.AugmentedPullRequestData -> String
pullRequestKey pullRequest =
    pullRequest.base.repo.fullName ++ ":" ++ toString pullRequest.number


pullRequestListToDict : List Model.AugmentedPullRequestData -> Model.PullRequestCollection
pullRequestListToDict pullRequests =
    let
        zippedList =
            List.map (\e -> ( pullRequestKey e, e )) pullRequests
    in
        Dict.fromList zippedList


issueUrlToDictKey : String -> String
issueUrlToDictKey url =
    issueUrlToRepository
        url
        ++ ":"
        ++ issueUrlToPullRequestId url


addLabels : Model.PullRequestCollection -> Model.IssuesData -> Model.PullRequestCollection
addLabels pullRequests issue =
    let
        key =
            issueUrlToDictKey issue.url

        pr : Maybe Model.AugmentedPullRequestData
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


addComments : Model.PullRequestCollection -> List Model.PullRequestCommentData -> Model.PullRequestCollection
addComments pullRequests comments =
    let
        key : String
        key =
            comments
                |> List.map (\e -> issueUrlToDictKey e.issueUrl)
                |> List.head
                |> Maybe.withDefault "error"

        pr : Maybe Model.AugmentedPullRequestData
        pr =
            Dict.get key pullRequests

        extractComments : Model.AugmentedPullRequestData -> List Model.PullRequestCommentData -> Model.AugmentedPullRequestData
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

        newPr : Maybe Model.AugmentedPullRequestData
        newPr =
            Maybe.map (\pull -> extractComments pull comments) pr
    in
        case newPr of
            Nothing ->
                pullRequests

            Just a ->
                Dict.union (Dict.singleton key a) pullRequests


updatePullRequests : Model.PullRequestCollection -> List Model.PullRequestData -> Model.PullRequestCollection
updatePullRequests pullRequests newPullRequests =
    Dict.union
        (newPullRequests
            |> List.map (\e -> augmentPullRequestData e)
            |> pullRequestListToDict
        )
        pullRequests
