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


pullRequestColor : Model.AugmentedPullRequestData -> Time.Time -> Float -> ( String, String )
pullRequestColor pullRequest decayTimeInDays elapsedTime =
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
        if pullRequest.state == "open" then
            if readyForMerge pullRequest then
                ( "background-color", "#65f442" )
            else
                ( "background-color", "hsl(0, 100%, " ++ toString lValue ++ "%)" )
        else
            ( "background-color", "#65f442" )


readyForMerge : Model.AugmentedPullRequestData -> Bool
readyForMerge pullRequest =
    let
        hasReadyForMergeLabel =
            ((List.filter (\label -> label.name == "Ready for Merge") pullRequest.labels) |> List.length) > 0

        hasEnoughThumbs =
            (pullRequest.comments |> List.length) >= 2

        lastBuildPassed =
            True
    in
        hasReadyForMergeLabel && hasEnoughThumbs && lastBuildPassed


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
                    [ pullRequestColor pullRequest model.decayTimeInDays elapsedTime
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


zeroExtend : String -> String
zeroExtend str =
    if (String.length str == 1) then
        "0" ++ str
    else
        str


dateString : Date.Date -> String
dateString date =
    toString (Date.month date)
        ++ " "
        ++ toString (Date.day date)
        ++ ", "
        ++ toString (Date.year date)
        ++ " -  "
        ++ (toString (Date.hour date) |> zeroExtend)
        ++ ":"
        ++ (toString (Date.minute date) |> zeroExtend)
        ++ ":"
        ++ (toString (Date.second date) |> zeroExtend)


currentTimeDisplay : Model -> Html Msg
currentTimeDisplay model =
    let
        date =
            Date.fromTime model.currentTime
    in
        h2 [ style [ ( "margin-bottom", "50px" ) ] ]
            [ span [ class "label label-primary" ] [ date |> dateString |> text ]
            ]


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
              {-
                 , decayDisplay model.decayTimeInDays
                 , decayForm
              -}
            , pullRequestTable model
            ]
        ]
