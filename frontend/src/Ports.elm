port module Ports exposing
    ( clearDebugInfo
    , copyToClipboard
    , getOrgSlug
    , onCopyResult
    , receiveOrgSlug
    , saveDebugInfo
    , viewingPhone
    )

-- Port for requesting the orgSlug from JavaScript


port getOrgSlug : () -> Cmd msg



-- Port for receiving the orgSlug from JavaScript


port receiveOrgSlug : (String -> msg) -> Sub msg



-- Port for copying text to clipboard


port copyToClipboard : String -> Cmd msg



-- Port for receiving copy result from JavaScript


port onCopyResult : (Bool -> msg) -> Sub msg



-- Port for saving debug info


port saveDebugInfo : String -> Cmd msg



-- Port for clearing debug info


port clearDebugInfo : () -> Cmd msg



-- Port for tracking when the user is viewing the phone preview


port viewingPhone : (Bool -> msg) -> Sub msg
