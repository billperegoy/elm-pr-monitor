module Config exposing (..)


repositories : List String
repositories =
    [ "es/contacts-core"
    , "contacts/contacts-listpicker-ui"
    , "es/smsjmml"
    , "es/ctct"
    ]


apiBase : String
apiBase =
    "https://github.roving.com/api/v3"


pullRequestUrl : String -> String
pullRequestUrl repository =
    apiBase
        ++ "/repos/"
        ++ repository
        ++ "/pulls"



-- These are pull request comments


commentsUrl : String -> Int -> String
commentsUrl repository pullRequestId =
    apiBase
        ++ "/repos/"
        ++ repository
        ++ "/issues/"
        ++ toString pullRequestId
        ++ "/comments"


issuesUrl : String -> Int -> String
issuesUrl repository pullRequestId =
    apiBase
        ++ "/repos/"
        ++ repository
        ++ "/issues/"
        ++ toString pullRequestId
