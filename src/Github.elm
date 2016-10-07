module Github exposing (..)

import Task exposing (..)
import Http exposing (..)
import Json.Decode exposing (int, string, float, Decoder)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


--

import DateUtils exposing (..)


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


pullRequestListDecoder : Decoder (List PullRequestData)
pullRequestListDecoder =
    Json.Decode.list pullRequestDataDecoder


headDecoder : Decoder HeadData
headDecoder =
    decode HeadData
        |> Json.Decode.Pipeline.required "repo" repoDecoder


userDecoder : Decoder UserData
userDecoder =
    decode UserData
        |> Json.Decode.Pipeline.required "login" Json.Decode.string


repoDecoder : Decoder RepoData
repoDecoder =
    decode RepoData
        |> Json.Decode.Pipeline.required "name" Json.Decode.string


pullRequestDataDecoder : Decoder PullRequestData
pullRequestDataDecoder =
    decode PullRequestData
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
