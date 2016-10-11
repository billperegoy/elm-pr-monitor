module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Dict
import Time
import Task
import Http
import Date
import String
import List
import TimeAgo
import Github
import Config
import DateTimeUtils
import Set


main : Program Never
main =
    App.program
        { init = initModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--
-- Model
--


type alias PullRequestCollection =
    Dict.Dict String Github.PullRequestDataWithComments


type alias Model =
    { currentTime : Time.Time
    , pullRequests : PullRequestCollection
    , decayTimeFormValue : String
    , decayTimeInDays : Float
    , errors : Maybe String
    }



--
-- Init
--


initModel : ( Model, Cmd Msg )
initModel =
    { currentTime = 0.0
    , pullRequests = Dict.empty
    , decayTimeFormValue = ""
    , decayTimeInDays = 5
    , errors = Nothing
    }
        ! getAllPullRequestData Config.repositories



--
-- Update
--


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


type Msg
    = PullRequestDataHttpFail Http.Error
    | PullRequestDataHttpSucceed (List Github.PullRequestData)
    | PullRequestCommentDataHttpFail Http.Error
    | PullRequestCommentDataHttpSucceed (List Github.PullRequestCommentData)
    | SetDecayTimeFormValue String
    | UpdateDecayTime
    | EverySecond Float
    | UpdatePullRequestData Float


pullRequestKey : Github.PullRequestDataWithComments -> String
pullRequestKey pullRequest =
    let
        repo =
            urlToRepository pullRequest.htmlUrl
    in
        repo ++ ":" ++ toString pullRequest.number


pullRequestListToDict : List Github.PullRequestDataWithComments -> PullRequestCollection
pullRequestListToDict pullRequests =
    let
        zippedList =
            List.map (\e -> ( pullRequestKey e, e )) pullRequests
    in
        Dict.fromList zippedList


addComments : Model -> List Github.PullRequestCommentData -> PullRequestCollection
addComments model comments =
    let
        key : Maybe String
        key =
            List.map
                (\e -> issueUrlToRepository e.issueUrl ++ ":" ++ issueUrlToPullRequestId e.issueUrl)
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
                    Dict.get a model.pullRequests

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
                model.pullRequests

            Just a ->
                Dict.union (Dict.singleton realKey a)
                    model.pullRequests


updatePullRequests : PullRequestCollection -> List Github.PullRequestData -> PullRequestCollection
updatePullRequests pullRequests newPullRequests =
    Dict.union
        (pullRequestListToDict
            (List.map (\e -> Github.addComments e) newPullRequests)
        )
        pullRequests


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PullRequestDataHttpSucceed pullRequests ->
            { model
                | pullRequests = updatePullRequests model.pullRequests pullRequests
            }
                ! getAllPullRequestCommentData pullRequests

        PullRequestDataHttpFail error ->
            { model | errors = Just (toString error) }
                ! []

        PullRequestCommentDataHttpSucceed comments ->
            { model | pullRequests = addComments model comments }
                ! []

        PullRequestCommentDataHttpFail error ->
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
    Task.perform
        PullRequestDataHttpFail
        PullRequestDataHttpSucceed
        (Http.get
            Github.pullRequestListDecoder
            (Config.pullRequestUrl repository)
        )


getAllPullRequestCommentData : List Github.PullRequestData -> List (Cmd Msg)
getAllPullRequestCommentData pullRequests =
    List.map
        (\pullRequest ->
            getPullRequestCommentData
                (urlToRepository pullRequest.htmlUrl)
                pullRequest.number
        )
        pullRequests


getPullRequestCommentData : String -> Int -> Cmd Msg
getPullRequestCommentData repository pullRequestId =
    Task.perform
        PullRequestCommentDataHttpFail
        PullRequestCommentDataHttpSucceed
        (Http.get
            Github.pullRequestCommentListDecoder
            (Config.commentsUrl repository pullRequestId)
        )



--
-- View
---


elapsedTimeToColor : String -> Time.Time -> Float -> ( String, String )
elapsedTimeToColor state decayTimeInDays elapsedTime =
    let
        decayTimeInSeconds =
            decayTimeInDays * 24 * 3600

        percentDone =
            Basics.min
                (100 * (Time.inSeconds elapsedTime) / decayTimeInSeconds)
                100

        percentLeft =
            100.0 - percentDone

        -- Want this to go from 50% down to 100% over time
        lValue =
            truncate (50.0 + (percentLeft / 2))
    in
        if state == "open" then
            ( "background-color", "hsl(0, 100%, " ++ toString lValue ++ "%)" )
        else
            ( "background-color", "#65f442" )


pullRequestViewElement : Model -> Github.PullRequestDataWithComments -> Html Msg
pullRequestViewElement model pullRequest =
    let
        prTime =
            DateTimeUtils.dateStringToTime pullRequest.createdAt

        elapsedTime =
            model.currentTime - prTime

        truncate64 str =
            if (String.length str) > 64 then
                String.slice 0 63 str ++ "..."
            else
                str
    in
        tr []
            [ td
                [ style
                    [ elapsedTimeToColor pullRequest.state model.decayTimeInDays elapsedTime
                    ]
                ]
                [ text (TimeAgo.timeAgoInWords elapsedTime) ]
            , td []
                [ a
                    [ href pullRequest.htmlUrl, target "_blank" ]
                    [ text (toString pullRequest.number)
                    ]
                ]
            , td [] [ text pullRequest.head.repo.name ]
            , td [] [ text pullRequest.user.login ]
            , td [] [ text (truncate64 pullRequest.body) ]
            , td []
                (List.map
                    (\comment -> div [] [ text ("👍" ++ "  " ++ comment.user.login) ])
                    pullRequest.comments
                )
            ]


pageHeader : Html Msg
pageHeader =
    div [ class "jumbotron" ]
        [ h1
            [ class "text-center" ]
            [ text "Elm Pull Request Monitor" ]
        ]


pullRequestTableHeader : Html Msg
pullRequestTableHeader =
    thead []
        [ tr []
            [ th [] [ text "Age" ]
            , th [] [ text "PR#" ]
            , th [] [ text "Repository" ]
            , th [] [ text "Owner" ]
            , th [] [ text "Description" ]
            , th [] [ text "Thumbs" ]
            ]
        ]


pullRequestTable : Model -> Html Msg
pullRequestTable model =
    let
        sortedPullRequests =
            Dict.values model.pullRequests
                |> List.sortWith Github.sortByCreatedAt
    in
        table [ class "table" ]
            [ pullRequestTableHeader
            , tbody []
                (List.map
                    (\pullRequest -> pullRequestViewElement model pullRequest)
                    sortedPullRequests
                )
            ]


currentTimeDisplay : Model -> Html Msg
currentTimeDisplay model =
    let
        timeString =
            "Current Time: " ++ toString (Date.fromTime model.currentTime)
    in
        p [] [ text timeString ]


errors : Model -> Html Msg
errors model =
    case model.errors of
        Nothing ->
            div [] []

        Just a ->
            div [ class "alert alert-danger" ] [ text a ]


decayDisplay : Float -> Html Msg
decayDisplay decayTimeInDays =
    div []
        [ text
            ("Current decay time (days): "
                ++ (toString
                        decayTimeInDays
                   )
            )
        ]


decayForm : Html Msg
decayForm =
    div [ class "form-group", style [ ( "width", "200px" ) ] ]
        [ label [ for "decayTimeInput" ] [ text "Set Decay Time" ]
        , input
            [ class "form-control"
            , id "decayTimeInput"
            , onInput SetDecayTimeFormValue
            ]
            []
        , button
            [ type' "submit"
            , class "btn btn-primary"
            , onClick UpdateDecayTime
            ]
            [ text "Submit" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ pageHeader
        , div [ class "container" ]
            [ errors model
            , currentTimeDisplay model
            , decayDisplay model.decayTimeInDays
            , decayForm
            , pullRequestTable model
            ]
        ]



--
-- Subscriptions
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every Time.second EverySecond
        , Time.every (5 * Time.minute) UpdatePullRequestData
        ]
