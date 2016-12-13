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
            xgetPullRequestData model pr

        GetPullRequestData (Err error) ->
            updateError model error

        GetPullRequestCommentData (Ok comments) ->
            xgetPullRequestCommentData model comments

        GetPullRequestCommentData (Err error) ->
            updateError model error

        GetPullRequestIssuesData (Ok issue) ->
            xgetPullRequestIssuesData model issue

        GetPullRequestIssuesData (Err error) ->
            updateError model error

        GetPullRequestStatusData (Ok statuses) ->
            nothing model

        GetPullRequestStatusData (Err error) ->
            updateError model error

        SetDecayTimeFormValue value ->
            setDecayTimeFormValue model value

        UpdateDecayTime ->
            updateDecayTime model

        EverySecond time ->
            everySecond model time

        UpdatePullRequestData _ ->
            model ! getAllPullRequestData repositories



-- FIXME - bad name


xgetPullRequestData model pr =
    { model
        | pullRequests = Github.updatePullRequests model.pullRequests pr
        , errors = Nothing
    }
        ! getPullRequestSubResources pr



-- FIXME - bad name


xgetPullRequestCommentData model comments =
    { model
        | pullRequests = Github.addComments model.pullRequests comments
        , errors = Nothing
    }
        ! []



-- FIXME - bad name


xgetPullRequestIssuesData model issue =
    { model
        | pullRequests = Github.addLabels model.pullRequests issue
        , errors = Nothing
    }
        ! []


setDecayTimeFormValue model value =
    { model | decayTimeFormValue = value } ! []


nothing : Model -> ( Model, Cmd Msg )
nothing model =
    model ! []


everySecond : Model -> Time.Time -> ( Model, Cmd Msg )
everySecond model time =
    { model | currentTime = time } ! []


updateDecayTime : Model -> ( Model, Cmd Msg )
updateDecayTime model =
    { model
        | decayTimeInDays =
            DateTimeUtils.timeStringToFloat
                model.decayTimeFormValue
                model.decayTimeInDays
    }
        ! []


updateError : Model -> Http.Error -> ( Model, Cmd Msg )
updateError model error =
    { model | errors = Just (toString error) } ! []



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
