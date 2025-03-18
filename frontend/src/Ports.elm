port module Ports exposing
    ( copyToClipboard
    , getOrgSlug
    , onCopyResult
    , receiveOrgSlug
    )

-- Port for requesting the orgSlug from JavaScript


port getOrgSlug : () -> Cmd msg


-- Port for receiving the orgSlug from JavaScript


port receiveOrgSlug : (String -> msg) -> Sub msg


-- Port for copying text to clipboard


port copyToClipboard : String -> Cmd msg


-- Port for receiving copy result from JavaScript


port onCopyResult : (Bool -> msg) -> Sub msg
