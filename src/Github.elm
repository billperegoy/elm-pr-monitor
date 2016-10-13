module Github exposing (..)

import Http
import Json.Decode
import Json.Decode.Pipeline
import String
import DateTimeUtils


type alias PullRequestData =
    { number : Int
    , htmlUrl : String
    , body : String
    , state : String
    , createdAt : String
    , head : HeadData
    , user : UserData
    }


type alias PullRequestDataWithComments =
    { number : Int
    , htmlUrl : String
    , body : String
    , state : String
    , createdAt : String
    , head : HeadData
    , user : UserData
    , comments : List PullRequestCommentData
    }


addComments : PullRequestData -> PullRequestDataWithComments
addComments elem =
    { number = elem.number
    , htmlUrl = elem.htmlUrl
    , body = elem.body
    , state = elem.state
    , createdAt = elem.createdAt
    , head = elem.head
    , user = elem.user
    , comments = []
    }


type alias PullRequestCommentData =
    { body : String
    , user : UserData
    , issueUrl : String
    }


type alias HeadData =
    { repo : RepoData }


type alias RepoData =
    { name : String }


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
        |> Json.Decode.Pipeline.required "body" Json.Decode.string
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "head" headDecoder
        |> Json.Decode.Pipeline.required "user" userDecoder


headDecoder : Json.Decode.Decoder HeadData
headDecoder =
    Json.Decode.Pipeline.decode HeadData
        |> Json.Decode.Pipeline.required "repo" repoDecoder


userDecoder : Json.Decode.Decoder UserData
userDecoder =
    Json.Decode.Pipeline.decode UserData
        |> Json.Decode.Pipeline.required "login" Json.Decode.string


repoDecoder : Json.Decode.Decoder RepoData
repoDecoder =
    Json.Decode.Pipeline.decode RepoData
        |> Json.Decode.Pipeline.required "name" Json.Decode.string


sortByCreatedAt : PullRequestDataWithComments -> PullRequestDataWithComments -> Order
sortByCreatedAt a b =
    compare (DateTimeUtils.dateStringToTime a.createdAt)
        (DateTimeUtils.dateStringToTime b.createdAt)


urlToRepository : String -> String
urlToRepository url =
    String.split "/" url
        |> List.drop 3
        |> List.take 2
        |> String.join "/"


issueUrlToRepository : String -> String
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
