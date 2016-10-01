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
    , errors : String
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
    , errors = "No errors"
    }
        ! List.map (\e -> getPullRequestData e) config.repositories


type alias Config =
    { repositories : List Repository
    }


config : Config
config =
    { repositories =
        [ Repository "rtfeldman" "node-elm-compiler"
        , Repository "rtfeldman" "elm-css"
        , Repository "rtfeldman" "elm-webpack-loader"
        , Repository "billperegoy" "elm-components"
        ]
    }


type Msg
    = NoOp
    | GetPullRequestData Repository
    | GetPullRequestDataHttpFail Http.Error
    | GetPullRequestDataHttpSucceed (List PullRequestData)
    | Tick Float


apiBase : String
apiBase =
    "https://api.github.com"


pullRequestUrl : Repository -> String
pullRequestUrl repo =
    apiBase ++ "/repos/" ++ repo.user ++ "/" ++ repo.project ++ "/pulls"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        GetPullRequestData repository ->
            model ! []

        GetPullRequestDataHttpSucceed results ->
            { model | pullRequests = model.pullRequests ++ results }
                ! []

        GetPullRequestDataHttpFail error ->
            { model | errors = toString error }
                ! []

        Tick time ->
            { model | currentTime = time } ! []


type alias Repository =
    { user : String
    , project : String
    }


repoViewElement : PullRequestData -> Html Msg
repoViewElement repository =
    tr []
        [ td [] [ text repository.head.repo.name ]
        , td [] [ text (toString repository.number) ]
        , td [] [ text repository.body ]
        , td [] [ text repository.created_at ]
        , td [] [ text repository.state ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ p []
            [ text model.errors ]
        , p [] [ text (toString (fromTime model.currentTime)) ]
        , table [ class "table" ]
            [ tbody []
                (List.map (\e -> repoViewElement e) model.pullRequests)
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ every Time.second Tick ]
