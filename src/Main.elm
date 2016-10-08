module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
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


type alias Model =
    { currentTime : Time.Time
    , pullRequests : List Github.PullRequestData
    , decayTimeFormValue : String
    , decayTimeInDays : Float
    , errors : Maybe String
    , comments : List Github.PullRequestCommentData
    }



--
-- Init
--


initModel : ( Model, Cmd Msg )
initModel =
    let
        config =
            Config.data
    in
        { currentTime = 0.0
        , pullRequests = []
        , decayTimeFormValue = ""
        , decayTimeInDays = 5
        , errors = Nothing
        , comments = []
        }
            ! List.map (\repo -> getPullRequestData repo) config.repositories



--
-- Update
--


urlToRepository : String -> Config.Repository
urlToRepository url =
    let
        userAndRepo =
            String.split "/" url
                |> List.drop 3
                |> List.take 2

        user =
            List.take 1 userAndRepo
                |> List.head
                |> Maybe.withDefault "error"

        repo =
            List.drop 1 userAndRepo
                |> List.head
                |> Maybe.withDefault "error"
    in
        Config.Repository user repo


type Msg
    = PullRequestDataHttpFail Http.Error
    | PullRequestDataHttpSucceed (List Github.PullRequestData)
    | PullRequestCommentDataHttpFail Http.Error
    | PullRequestCommentDataHttpSucceed (List Github.PullRequestCommentData)
    | SetDecayTimeFormValue String
    | UpdateDecayTime
    | EverySecond Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PullRequestDataHttpSucceed results ->
            { model | pullRequests = model.pullRequests ++ results }
                ! List.map
                    (\pullRequest ->
                        getPullRequestCommentData
                            (urlToRepository pullRequest.htmlUrl)
                            pullRequest.number
                    )
                    results

        PullRequestDataHttpFail error ->
            { model | errors = Just (toString error) }
                ! []

        PullRequestCommentDataHttpSucceed results ->
            { model | comments = model.comments ++ results } ! []

        PullRequestCommentDataHttpFail results ->
            model ! []

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



--
-- Http
--


getPullRequestData : Config.Repository -> Cmd Msg
getPullRequestData repository =
    Task.perform
        PullRequestDataHttpFail
        PullRequestDataHttpSucceed
        (Http.get
            Github.pullRequestListDecoder
            (Config.pullRequestUrl repository)
        )


getPullRequestCommentData : Config.Repository -> Int -> Cmd Msg
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


elapsedTimeToColor : Time.Time -> Float -> ( String, String )
elapsedTimeToColor decayTimeInDays elapsedTime =
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
        ( "background-color", "hsl(0, 100%, " ++ toString lValue ++ "%)" )


pullRequestViewElement : Model -> Github.PullRequestData -> Html Msg
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
                    [ elapsedTimeToColor model.decayTimeInDays elapsedTime
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
            ]
        ]


pullRequestTable : Model -> Html Msg
pullRequestTable model =
    let
        sortedPullRequests =
            List.sortWith Github.sortByCreatedAt model.pullRequests
    in
        table [ class "table" ]
            [ pullRequestTableHeader
            , tbody []
                (List.map
                    (\pulRequest -> pullRequestViewElement model pulRequest)
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


comments : Model -> Html Msg
comments model =
    ul []
        (List.filter (\e -> String.contains "👍" e.body) model.comments
            |> List.map (\e -> li [] [ text (e.body ++ " (" ++ e.user.login ++ ")") ])
        )


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
            , comments model
            ]
        ]



--
-- Subscriptions
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Time.every Time.second EverySecond ]
