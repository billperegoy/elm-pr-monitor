module ModelUpdate exposing (..)

import Dict
import List
import Set
import String


--

import Github


type alias PullRequestCollection =
    Dict.Dict String Github.PullRequestDataWithComments


pullRequestKey : Github.PullRequestDataWithComments -> String
pullRequestKey pullRequest =
    let
        repo =
            Github.urlToRepository pullRequest.htmlUrl
    in
        repo ++ ":" ++ toString pullRequest.number


pullRequestListToDict : List Github.PullRequestDataWithComments -> PullRequestCollection
pullRequestListToDict pullRequests =
    let
        zippedList =
            List.map (\e -> ( pullRequestKey e, e )) pullRequests
    in
        Dict.fromList zippedList


addLabels : PullRequestCollection -> List Github.IssuesData -> PullRequestCollection
addLabels pullRequests issues =
    let
        key : String
        key =
            issues
                |> List.map
                    (\e ->
                        Github.issueUrlToRepository e.url
                            ++ ":"
                            ++ Github.issueUrlToPullRequestId e.url
                    )
                |> List.head
                |> Maybe.withDefault "error"

        pr : Maybe Github.PullRequestDataWithComments
        pr =
            Dict.get key pullRequests
    in
        pullRequests


addComments : PullRequestCollection -> List Github.PullRequestCommentData -> PullRequestCollection
addComments pullRequests comments =
    let
        key : String
        key =
            comments
                |> List.map
                    (\e ->
                        Github.issueUrlToRepository e.issueUrl
                            ++ ":"
                            ++ Github.issueUrlToPullRequestId e.issueUrl
                    )
                |> List.head
                |> Maybe.withDefault "error"

        pr : Maybe Github.PullRequestDataWithComments
        pr =
            Dict.get key pullRequests

        newPr : Maybe Github.PullRequestDataWithComments
        newPr =
            case pr of
                Nothing ->
                    Nothing

                Just a ->
                    Just
                        { a
                            | comments =
                                List.filter
                                    (\e ->
                                        (String.contains "👍" e.body)
                                            || (String.contains ":+1" e.body)
                                    )
                                    comments
                        }
    in
        case newPr of
            Nothing ->
                pullRequests

            Just a ->
                Dict.union (Dict.singleton key a)
                    pullRequests


updatePullRequests : PullRequestCollection -> List Github.PullRequestData -> PullRequestCollection
updatePullRequests pullRequests newPullRequests =
    Dict.union
        (newPullRequests
            |> List.map (\e -> Github.addComments e)
            |> pullRequestListToDict
        )
        pullRequests
