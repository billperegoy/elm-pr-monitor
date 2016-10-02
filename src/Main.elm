module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Task exposing (..)
import Http exposing (..)
import Json.Decode exposing (int, string, float, Decoder)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Time exposing (..)
import Date exposing (..)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { currentTime : Float
    , pullRequests : List PullRequestData
    , errors : Maybe String
    }


type alias PullRequestData =
    { number : Int
    , body : String
    , state : String
    , created_at : String
    , head : HeadData
    }


type alias HeadData =
    { repo : RepoData }


type alias RepoData =
    { name : String }


repoDecoder : Decoder RepoData
repoDecoder =
    decode RepoData
        |> Json.Decode.Pipeline.required "name" Json.Decode.string


headDecoder : Decoder HeadData
headDecoder =
    decode HeadData
        |> Json.Decode.Pipeline.required "repo" repoDecoder


pullRequestListDecoder : Decoder (List PullRequestData)
pullRequestListDecoder =
    Json.Decode.list pullRequestDataDecoder


pullRequestDataDecoder : Decoder PullRequestData
pullRequestDataDecoder =
    decode PullRequestData
        |> Json.Decode.Pipeline.required "number" Json.Decode.int
        |> Json.Decode.Pipeline.required "body" Json.Decode.string
        |> Json.Decode.Pipeline.required "state" Json.Decode.string
        |> Json.Decode.Pipeline.required "created_at" Json.Decode.string
        |> Json.Decode.Pipeline.required "head" headDecoder


getPullRequestData : Repository -> Cmd Msg
getPullRequestData repository =
    Task.perform
        GetPullRequestDataHttpFail
        GetPullRequestDataHttpSucceed
        (Http.get pullRequestListDecoder (pullRequestUrl repository))


init : ( Model, Cmd Msg )
init =
    { currentTime = 0.0
    , pullRequests = []
    , errors = Nothing
    }
        ! List.map (\e -> getPullRequestData e) config.repositories


type alias Config =
    { repositories : List Repository
    }


config : Config
config =
    { repositories =
        [ --Repository "rtfeldman" "node-elm-compiler"
          --, Repository "rtfeldman" "elm-css"
          --, Repository "rtfeldman" "elm-webpack-loader"
          --, Repository "billperegoy" "elm-components"
          Repository "billperegoy" "elm-pr-monitor"
        ]
    }


type Msg
    = GetPullRequestData Repository
    | GetPullRequestDataHttpFail Http.Error
    | GetPullRequestDataHttpSucceed (List PullRequestData)
    | Tick Float


apiBase : String
apiBase =
    "https://api.github.com"


pullRequestUrl : Repository -> String
pullRequestUrl repo =
    apiBase
        ++ "/repos/"
        ++ repo.user
        ++ "/"
        ++ repo.project
        ++ "/pulls"


commentsUrl : Repository -> Int -> String
commentsUrl repository pullRequestId =
    apiBase
        ++ "/repos/"
        ++ repository.user
        ++ "/"
        ++ repository.project
        ++ "/issues/`"
        ++ toString pullRequestId
        ++ "/comments"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPullRequestData repository ->
            model ! []

        GetPullRequestDataHttpSucceed results ->
            { model | pullRequests = model.pullRequests ++ results }
                ! []

        GetPullRequestDataHttpFail error ->
            { model | errors = Just (toString error) }
                ! []

        Tick time ->
            { model | currentTime = time } ! []


type alias Repository =
    { user : String
    , project : String
    }


max : Float -> Float -> Float
max val max =
    if val > max then
        max
    else
        val


elapsedTimeToColor : Float -> ( String, String )
elapsedTimeToColor elapsedTime =
    let
        decayTimeInSeconds =
            10000

        percentDone =
            max (100 * (inSeconds elapsedTime) / decayTimeInSeconds) 100

        percentLeft =
            100.0 - percentDone

        -- Want this to go from 50% down to 100% over time
        lValue =
            truncate (50.0 + (percentLeft / 2))
    in
        ( "background-color", "hsl(0, 100%, " ++ toString lValue ++ "%)" )


repoViewElement : Model -> PullRequestData -> Html Msg
repoViewElement model repository =
    let
        prTime =
            dateStringToTime repository.created_at

        elapsedTime =
            model.currentTime - prTime
    in
        tr [ style [ elapsedTimeToColor elapsedTime ] ]
            [ td [] [ text repository.head.repo.name ]
            , td [] [ text (toString repository.number) ]
            , td [] [ text repository.body ]
            , td [] [ text repository.created_at ]
            , td [] [ text repository.state ]
            ]


pageHeader : Html Msg
pageHeader =
    div [ class "jumbotron" ]
        [ h1 [ class "text-center" ] [ text "Elm Pull Request Monitor" ]
        ]


pullRequestTableHeader : Html Msg
pullRequestTableHeader =
    thead []
        [ tr []
            [ td [] [ text "Repo" ]
            , td [] [ text "PR#" ]
            , td [] [ text "Description" ]
            , td [] [ text "Date" ]
            , td [] [ text "State" ]
            ]
        ]


pullRequestTable : Model -> Html Msg
pullRequestTable model =
    table [ class "table" ]
        [ pullRequestTableHeader
        , tbody []
            (List.map (\e -> repoViewElement model e) model.pullRequests)
        ]


dateStringToTime : String -> Time
dateStringToTime dateString =
    let
        dateResult =
            Date.fromString dateString
    in
        case dateResult of
            Ok value ->
                toTime value

            Err _ ->
                0.0


currentTime : Model -> Html Msg
currentTime model =
    let
        timeString =
            "Current Time: " ++ toString (fromTime model.currentTime)
    in
        p [] [ text timeString ]


errors : Model -> Html Msg
errors model =
    case model.errors of
        Nothing ->
            p [] []

        Just a ->
            p [] [ text a ]


view : Model -> Html Msg
view model =
    div []
        [ pageHeader
        , div [ class "container" ]
            [ errors model
            , currentTime model
            , pullRequestTable model
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ every Time.second Tick ]
