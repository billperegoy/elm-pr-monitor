module Github exposing (..)

import Http
import Json.Decode
import Json.Decode.Pipeline
import String
import DateTimeUtils
import Dict


type alias StatusData =
    { url : String
    , description : String
    , state : String
    }


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


type BuildStatus
    = Pending
    | Success
    | Fail


augmentPullRequestData : PullRequestData -> AugmentedPullRequestData
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
    , buildStatus = Fail
    }


type alias PullRequestLabel =
    { name : String
    , color : String
    }


type alias IssuesData =
    { url : String
    , number : Int
    , labels : List PullRequestLabel
    }


issuesDecoder : Json.Decode.Decoder IssuesData
issuesDecoder =
    Json.Decode.Pipeline.decode IssuesData
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "number" Json.Decode.int
        |> Json.Decode.Pipeline.required "labels" pullRequestLabelListDecoder


statusListDecoder : Json.Decode.Decoder (List StatusData)
statusListDecoder =
    Json.Decode.list statusDecoder


statusDecoder : Json.Decode.Decoder StatusData
statusDecoder =
    Json.Decode.Pipeline.decode StatusData
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "state" Json.Decode.string


pullRequestLabelListDecoder : Json.Decode.Decoder (List PullRequestLabel)
pullRequestLabelListDecoder =
    Json.Decode.list pullRequestLabelDecoder


pullRequestLabelDecoder : Json.Decode.Decoder PullRequestLabel
pullRequestLabelDecoder =
    Json.Decode.Pipeline.decode PullRequestLabel
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "color" Json.Decode.string


type alias PullRequestCommentData =
    { body : String
    , user : UserData
    , issueUrl : String
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


pullRequestCommentListDecoder : Json.Decode.Decoder (List PullRequestCommentData)
pullRequestCommentListDecoder =
    Json.Decode.list pullRequestCommentDecoder


pullRequestCommentDecoder : Json.Decode.Decoder PullRequestCommentData
pullRequestCommentDecoder =
    Json.Decode.Pipeline.decode PullRequestCommentData
        |> Json.Decode.Pipeline.required "body" Json.Decode.string
        |> Json.Decode.Pipeline.required "user" userDecoder
        |> Json.Decode.Pipeline.required "issue_url" Json.Decode.string


pullRequestListDecoder : Json.Decode.Decoder (List PullRequestData)
pullRequestListDecoder =
    Json.Decode.list pullRequestDataDecoder


pullRequestDataDecoder : Json.Decode.Decoder PullRequestData
pullRequestDataDecoder =
    Json.Decode.Pipeline.decode PullRequestData
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


gitElementDecoder : Json.Decode.Decoder GitElementData
gitElementDecoder =
    Json.Decode.Pipeline.decode GitElementData
        |> Json.Decode.Pipeline.required "repo" repoDecoder


userDecoder : Json.Decode.Decoder UserData
userDecoder =
    Json.Decode.Pipeline.decode UserData
        |> Json.Decode.Pipeline.required "login" Json.Decode.string


repoDecoder : Json.Decode.Decoder RepoData
repoDecoder =
    Json.Decode.Pipeline.decode RepoData
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "full_name" Json.Decode.string


sortByCreatedAt : AugmentedPullRequestData -> AugmentedPullRequestData -> Order
sortByCreatedAt a b =
    compare (DateTimeUtils.dateStringToTime a.createdAt)
        (DateTimeUtils.dateStringToTime b.createdAt)


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


type alias PullRequestCollection =
    Dict.Dict String AugmentedPullRequestData


pullRequestKey : AugmentedPullRequestData -> String
pullRequestKey pullRequest =
    pullRequest.base.repo.fullName ++ ":" ++ toString pullRequest.number


pullRequestListToDict : List AugmentedPullRequestData -> PullRequestCollection
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


addLabels : PullRequestCollection -> IssuesData -> PullRequestCollection
addLabels pullRequests issue =
    let
        key =
            issueUrlToDictKey issue.url

        pr : Maybe AugmentedPullRequestData
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


addComments : PullRequestCollection -> List PullRequestCommentData -> PullRequestCollection
addComments pullRequests comments =
    let
        key : String
        key =
            comments
                |> List.map (\e -> issueUrlToDictKey e.issueUrl)
                |> List.head
                |> Maybe.withDefault "error"

        pr : Maybe AugmentedPullRequestData
        pr =
            Dict.get key pullRequests

        extractComments : AugmentedPullRequestData -> List PullRequestCommentData -> AugmentedPullRequestData
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

        newPr : Maybe AugmentedPullRequestData
        newPr =
            Maybe.map (\pull -> extractComments pull comments) pr
    in
        case newPr of
            Nothing ->
                pullRequests

            Just a ->
                Dict.union (Dict.singleton key a) pullRequests


updatePullRequests : PullRequestCollection -> List PullRequestData -> PullRequestCollection
updatePullRequests pullRequests newPullRequests =
    Dict.union
        (newPullRequests
            |> List.map (\e -> augmentPullRequestData e)
            |> pullRequestListToDict
        )
        pullRequests
