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


addComments : PullRequestCollection -> List Github.PullRequestCommentData -> PullRequestCollection
addComments pullRequests comments =
    let
        key : Maybe String
        key =
            List.map
                (\e -> Github.issueUrlToRepository e.issueUrl ++ ":" ++ Github.issueUrlToPullRequestId e.issueUrl)
                comments
                |> Set.fromList
                |> Set.toList
                |> List.head

        realKey : String
        realKey =
            Maybe.withDefault "error" key

        pr : Maybe Github.PullRequestDataWithComments
        pr =
            case key of
                Nothing ->
                    Nothing

                Just a ->
                    Dict.get a pullRequests

        newPr : Maybe Github.PullRequestDataWithComments
        newPr =
            case pr of
                Nothing ->
                    Nothing

                Just a ->
                    Just { a | comments = List.filter (\e -> String.contains "👍" e.body) comments }
    in
        case newPr of
            Nothing ->
                pullRequests

            Just a ->
                Dict.union (Dict.singleton realKey a)
                    pullRequests


updatePullRequests : PullRequestCollection -> List Github.PullRequestData -> PullRequestCollection
updatePullRequests pullRequests newPullRequests =
    Dict.union
        (pullRequestListToDict
            (List.map (\e -> Github.addComments e) newPullRequests)
        )
        pullRequests
