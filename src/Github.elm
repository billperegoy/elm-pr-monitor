module Github exposing (..)

import Http
import Json.Decode
import Json.Decode.Pipeline


--

import DateUtils


type alias PullRequestData =
    { number : Int
    , htmlUrl : String
    , body : String
    , state : String
    , createdAt : String
    , head : HeadData
    , user : UserData
    }


type alias HeadData =
    { repo : RepoData }


type alias RepoData =
    { name : String }


type alias UserData =
    { login : String
    }


pullRequestListDecoder : Json.Decode.Decoder (List PullRequestData)
pullRequestListDecoder =
    Json.Decode.list pullRequestDataDecoder


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


sortByCreatedAt : PullRequestData -> PullRequestData -> Order
sortByCreatedAt a b =
    compare (DateUtils.dateStringToTime a.createdAt)
        (DateUtils.dateStringToTime b.createdAt)
