module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import Time
import Task
import Http
import Date
import String


--

import TimeAgo
import Github
import DateTimeUtils
import Model exposing (..)
import View


main : Program Never Model Msg
main =
    Html.program
        { init = initModel ! getAllPullRequestData repositories
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }


repositories : List String
repositories =
    [ "ES/contacts-core"
    , "contacts/contacts-listpicker-ui"
    , "ES/ctct"
    ]



--
-- Update
--


getPullRequestSubResources : List Github.PullRequestData -> List (Cmd Msg)
getPullRequestSubResources pullRequests =
    getAllPullRequestCommentData pullRequests
        ++ getAllPullRequestIssuesData pullRequests
        ++ getAllPullRequestStatusData pullRequests


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPullRequestData (Ok pr) ->
            { model
                | pullRequests = Github.updatePullRequests model.pullRequests pr
                , errors = Nothing
            }
                ! getPullRequestSubResources pr

        GetPullRequestData (Err error) ->
            { model | errors = Just (toString error) } ! []

        GetPullRequestCommentData (Ok comments) ->
            { model
                | pullRequests = Github.addComments model.pullRequests comments
                , errors = Nothing
            }
                ! []

        GetPullRequestCommentData (Err error) ->
            { model | errors = Just (toString error) } ! []

        GetPullRequestIssuesData (Ok issue) ->
            { model
                | pullRequests = Github.addLabels model.pullRequests issue
                , errors = Nothing
            }
                ! []

        GetPullRequestIssuesData (Err error) ->
            { model | errors = Just (toString error) } ! []

        GetPullRequestStatusData (Ok statuses) ->
            model ! []

        GetPullRequestStatusData (Err error) ->
            { model | errors = Just (toString error) } ! []

        SetDecayTimeFormValue value ->
            { model | decayTimeFormValue = value } ! []

        UpdateDecayTime ->
            { model
                | decayTimeInDays =
                    DateTimeUtils.timeStringToFloat
                        model.decayTimeFormValue
                        model.decayTimeInDays
            }
                ! []

        EverySecond time ->
            { model | currentTime = time } ! []

        UpdatePullRequestData _ ->
            model ! getAllPullRequestData repositories



--
-- Http
--


getAllPullRequestData : List String -> List (Cmd Msg)
getAllPullRequestData repositories =
    List.map (\repo -> getPullRequestData repo) repositories


getPullRequestData : String -> Cmd Msg
getPullRequestData repository =
    let
        url =
            "https://github.roving.com/api/v3"
                ++ "/repos/"
                ++ repository
                ++ "/pulls"
    in
        Http.send GetPullRequestData
            (Http.get url Github.pullRequestListDecoder)



-----


getAllPullRequestCommentData : List Github.PullRequestData -> List (Cmd Msg)
getAllPullRequestCommentData pullRequests =
    List.map
        (\pr -> getPullRequestCommentData pr.commentsUrl)
        pullRequests


getPullRequestCommentData : String -> Cmd Msg
getPullRequestCommentData url =
    Http.send GetPullRequestCommentData
        (Http.get url Github.pullRequestCommentListDecoder)



-----


getAllPullRequestIssuesData : List Github.PullRequestData -> List (Cmd Msg)
getAllPullRequestIssuesData pullRequests =
    List.map
        (\pr -> getPullRequestIssuesData pr.issueUrl)
        pullRequests


getPullRequestIssuesData : String -> Cmd Msg
getPullRequestIssuesData url =
    Http.send GetPullRequestIssuesData
        (Http.get url Github.issuesDecoder)



-----


getAllPullRequestStatusData : List Github.PullRequestData -> List (Cmd Msg)
getAllPullRequestStatusData pullRequests =
    List.map
        (\pr -> getPullRequestStatusData pr.statusesUrl)
        pullRequests


getPullRequestStatusData : String -> Cmd Msg
getPullRequestStatusData url =
    Http.send GetPullRequestStatusData
        (Http.get url Github.statusListDecoder)



--
-- Subscriptions
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every Time.second EverySecond
        , Time.every (5 * Time.minute) UpdatePullRequestData
        ]
