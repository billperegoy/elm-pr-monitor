module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Task exposing (..)
import Http exposing (..)
import Time exposing (..)
import Date exposing (..)
import String exposing (..)
import List exposing (..)


--

import TimeAgo exposing (..)
import Github exposing (..)
import Config exposing (..)
import DateUtils exposing (..)


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
    { currentTime : Time
    , pullRequests : List Github.PullRequestData
    , decayTimeFormValue : String
    , decayTimeInDays : Float
    , errors : Maybe String
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
        }
            ! List.map (\repo -> getPullRequestData repo) config.repositories



--
-- Update
--


type Msg
    = PullRequestDataHttpFail Http.Error
    | PullRequestDataHttpSucceed (List Github.PullRequestData)
    | SetDecayTimeFormValue String
    | UpdateDecayTime
    | EverySecond Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PullRequestDataHttpSucceed results ->
            { model | pullRequests = model.pullRequests ++ results }
                ! []

        PullRequestDataHttpFail error ->
            { model | errors = Just (toString error) }
                ! []

        SetDecayTimeFormValue value ->
            { model | decayTimeFormValue = value } ! []

        UpdateDecayTime ->
            { model
                | decayTimeInDays =
                    decayTimeToFloat model.decayTimeFormValue model.decayTimeInDays
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
        (Http.get Github.pullRequestListDecoder (Config.pullRequestUrl repository))


max : Float -> Float -> Float
max val max =
    if val > max then
        max
    else
        val


decayTimeToFloat : String -> Float -> Float
decayTimeToFloat string default =
    let
        convertedValue =
            String.toFloat string
    in
        case convertedValue of
            Ok value ->
                value

            Err a ->
                default


elapsedTimeToColor : Time -> Float -> ( String, String )
elapsedTimeToColor decayTimeInDays elapsedTime =
    let
        decayTimeInSeconds =
            decayTimeInDays * 24 * 3600

        percentDone =
            max (100 * (inSeconds elapsedTime) / decayTimeInSeconds) 100

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
            DateUtils.dateStringToTime pullRequest.createdAt

        elapsedTime =
            model.currentTime - prTime
    in
        tr []
            [ td
                [ style
                    [ elapsedTimeToColor model.decayTimeInDays elapsedTime
                    ]
                ]
                [ text (timeAgoInWords elapsedTime) ]
            , td []
                [ a
                    [ href pullRequest.htmlUrl, target "_blank" ]
                    [ text (toString pullRequest.number)
                    ]
                ]
            , td [] [ text pullRequest.head.repo.name ]
            , td [] [ text pullRequest.user.login ]
            , td [] [ text (slice 0 63 pullRequest.body) ]
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
            sortWith sortByCreatedAt model.pullRequests
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
            "Current Time: " ++ toString (fromTime model.currentTime)
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ every Time.second EverySecond ]
