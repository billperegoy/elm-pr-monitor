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
import Config
import DateTimeUtils
import Model exposing (..)
import View


main : Program Never Model Msg
main =
    Html.program
        { init = initModel ! getAllPullRequestData Config.repositories
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }



--
-- Update
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPullRequestData result ->
            case result of
                Ok newPullRequests ->
                    { model
                        | pullRequests =
                            updatePullRequests model.pullRequests newPullRequests
                        , errors = Nothing
                    }
                        ! (getAllPullRequestCommentData newPullRequests
                            ++ getAllPullRequestIssuesData newPullRequests
                            ++ getAllPullRequestStatusData newPullRequests
                          )

                Err error ->
                    { model | errors = Just (toString error) } ! []

        GetPullRequestCommentData result ->
            case result of
                Ok comments ->
                    { model
                        | pullRequests = addComments model.pullRequests comments
                        , errors = Nothing
                    }
                        ! []

                Err error ->
                    { model | errors = Just (toString error) } ! []

        GetPullRequestIssuesData result ->
            case result of
                Ok issue ->
                    { model
                        | pullRequests =
                            addLabels
                                model.pullRequests
                                issue
                        , errors = Nothing
                    }
                        ! []

                Err error ->
                    { model | errors = Just (toString error) } ! []

        GetPullRequestStatusData result ->
            case result of
                Ok statuses ->
                    model ! []

                Err error ->
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
            model
                ! getAllPullRequestData Config.repositories



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
            Config.pullRequestUrl repository
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
