module View exposing (view)

import Date
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time


--

import DateTimeUtils
import TimeAgo
import Model exposing (..)


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


pullRequestViewElement : Model -> Model.AugmentedPullRequestData -> Html Msg
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
                    pullRequest.comments
                )
            , td [] (labelList pullRequest.labels)
            , td [] [ pullRequest.buildStatus |> buildStatusGlyphicon ]
            ]


buildStatusGlyphicon : Model.BuildStatus -> Html Msg
buildStatusGlyphicon status =
    case status of
        Model.Pending ->
            span
                [ style
                    [ ( "color", "black" ) ]
                , class "glyphicon glyphicon-refresh"
                ]
                []

        Model.Success ->
            span
                [ style
                    [ ( "color", "green" ) ]
                , class "glyphicon glyphicon-ok"
                ]
                []

        Model.Fail ->
            span
                [ style
                    [ ( "color", "red" ) ]
                , class "glyphicon glyphicon-remove"
                ]
                []


labelList : List Model.PullRequestLabel -> List (Html Msg)
labelList labels =
    List.map
        (\label ->
            div
                [ style
                    [ ( "background-color", label.color )
                    , ( "margin-bottom", "2px" )
                    , ( "display", "block" )
                    ]
                , class "label label-primary"
                ]
                [ text label.name ]
        )
        labels


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
            , th [] [ text "Labels" ]
            , th [] [ text "Build" ]
            ]
        ]


sortByCreatedAt : Model.AugmentedPullRequestData -> Model.AugmentedPullRequestData -> Order
sortByCreatedAt a b =
    compare (DateTimeUtils.dateStringToTime a.createdAt)
        (DateTimeUtils.dateStringToTime b.createdAt)


pullRequestTable : Model -> Html Msg
pullRequestTable model =
    let
        sortedPullRequests =
            Dict.values model.pullRequests
                |> List.sortWith sortByCreatedAt
    in
        table [ class "table table-striped" ]
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
