module Config exposing (..)


type alias Config =
    { repositories : List Repository
    }


type alias Repository =
    { user : String
    , project : String
    }


data : Config
data =
    { repositories =
        [ Repository "es" "contacts-core"
        , Repository "contacts" "contacts-listpicker-ui"
        , Repository "es" "smsjmml"
        ]
    }


apiBase : String
apiBase =
    "https://github.roving.com/api/v3"


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
