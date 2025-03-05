module ChangePlan exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import ChoosePlan
import Components.LimitBanner as LimitBanner exposing (LimitWarning(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode


type alias Model =
    { choosePlanModel : ChoosePlan.Model
    , showLimitBanner : Bool
    }


type Msg
    = ChoosePlanMsg ChoosePlan.Msg
    | CloseLimitBanner


init : { key : Nav.Key, session : String, orgSlug : String } -> ( Model, Cmd Msg )
init { key, session, orgSlug } =
    let
        ( choosePlanModel, choosePlanCmd ) =
            ChoosePlan.init orgSlug session key True
    in
    ( { choosePlanModel = choosePlanModel
      , showLimitBanner = True
      }
    , Cmd.map ChoosePlanMsg choosePlanCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChoosePlanMsg choosePlanMsg ->
            let
                ( updatedChoosePlanModel, choosePlanCmd ) =
                    ChoosePlan.update choosePlanMsg model.choosePlanModel
            in
            ( { model | choosePlanModel = updatedChoosePlanModel }
            , Cmd.map ChoosePlanMsg choosePlanCmd
            )

        CloseLimitBanner ->
            ( { model | showLimitBanner = False }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    { title = "Change Plan - Medicare Max"
    , body =
        [ if model.showLimitBanner then
            getLimitBanner

          else
            text ""
        , ChoosePlan.viewChangePlan model.choosePlanModel
            |> Html.map ChoosePlanMsg
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ChoosePlanMsg (ChoosePlan.subscriptions model.choosePlanModel)


getLimitBanner : Html Msg
getLimitBanner =
    let
        currentAgents =
            2

        agentLimit =
            1
    in
    LimitBanner.viewLimitBanner
        (Just (AgentLimit currentAgents agentLimit))
        CloseLimitBanner
