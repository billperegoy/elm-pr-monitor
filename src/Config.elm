module Config exposing (..)


type alias Config =
    { repositories : List String
    }


data : Config
data =
    { repositories =
        [ "es/contacts-core"
        , "contacts/contacts-listpicker-ui"
        , "es/smsjmml"
        ]
    }


apiBase : String
apiBase =
    "https://github.roving.com/api/v3"


pullRequestUrl : String -> String
pullRequestUrl repository =
    apiBase
        ++ "/repos/"
        ++ repository
        ++ "/pulls"


commentsUrl : String -> Int -> String
commentsUrl repository pullRequestId =
    apiBase
        ++ "/repos/"
        ++ repository
        ++ "/issues/"
        ++ toString pullRequestId
        ++ "/comments"
