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
import ModelUpdate


main : Program Never Model Msg
main =
    Html.program
        { init = initModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--
-- Model
--


type alias Model =
    { currentTime : Time.Time
    , pullRequests : ModelUpdate.PullRequestCollection
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


type Msg
    = GetPullRequestData (Result Http.Error (List Github.PullRequestData))
    | GetPullRequestCommentData (Result Http.Error (List Github.PullRequestCommentData))
    | SetDecayTimeFormValue String
    | UpdateDecayTime
    | EverySecond Float
    | UpdatePullRequestData Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPullRequestData result ->
            case result of
                Ok newPullRequests ->
                    { model
                        | pullRequests =
                            ModelUpdate.updatePullRequests model.pullRequests newPullRequests
                        , errors = Nothing
                    }
                        ! getAllPullRequestCommentData newPullRequests

                Err error ->
                    { model | errors = Just (toString error) } ! []

        GetPullRequestCommentData result ->
            case result of
                Ok comments ->
                    { model
                        | pullRequests = ModelUpdate.addComments model.pullRequests comments
                        , errors = Nothing
                    }
                        ! []

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


getAllPullRequestCommentData : List Github.PullRequestData -> List (Cmd Msg)
getAllPullRequestCommentData pullRequests =
    List.map
        (\pullRequest ->
            getPullRequestCommentData
                (Github.urlToRepository pullRequest.htmlUrl)
                pullRequest.number
        )
        pullRequests


getPullRequestCommentData : String -> Int -> Cmd Msg
getPullRequestCommentData repository pullRequestId =
    let
        url =
            Config.commentsUrl repository pullRequestId
    in
        Http.send GetPullRequestCommentData
            (Http.get url Github.pullRequestCommentListDecoder)



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
            , td [] [ text (truncate64 pullRequest.title) ]
            , td []
                (List.map
                    (\comment -> div [] [ text ("👍" ++ "  " ++ comment.user.login) ])
                    (Debug.log "Comments: " pullRequest.comments)
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
            [ type_ "submit"
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
