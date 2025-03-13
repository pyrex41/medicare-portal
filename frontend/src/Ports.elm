port module Ports exposing
    ( getOrgSlug
    , receiveOrgSlug
    )

-- Port for requesting the orgSlug from JavaScript


port getOrgSlug : () -> Cmd msg



-- Port for receiving the orgSlug from JavaScript


port receiveOrgSlug : (String -> msg) -> Sub msg
