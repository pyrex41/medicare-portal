module ChangePlan exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import ChoosePlan
import Components.LimitBanner as LimitBanner
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode


type alias Model =
    { choosePlanModel : ChoosePlan.Model
    , limitBanner : LimitBanner.Model
    }


type Msg
    = ChoosePlanMsg ChoosePlan.Msg
    | ChooseBannerMsg LimitBanner.Msg


init : { key : Nav.Key, session : String, orgSlug : String } -> ( Model, Cmd Msg )
init { key, session, orgSlug } =
    let
        ( choosePlanModel, choosePlanCmd ) =
            ChoosePlan.init orgSlug session key True

        ( limitBannerModel, limitBannerCmd ) =
            LimitBanner.init
    in
    ( { choosePlanModel = choosePlanModel
      , limitBanner = limitBannerModel
      }
    , Cmd.batch
        [ Cmd.map ChoosePlanMsg choosePlanCmd
        , Cmd.map ChooseBannerMsg limitBannerCmd
        ]
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

        ChooseBannerMsg chooseBannerMsg ->
            let
                ( updatedChooseBannerModel, chooseBannerCmd ) =
                    LimitBanner.update chooseBannerMsg model.limitBanner
            in
            ( { model | limitBanner = updatedChooseBannerModel }
            , Cmd.map ChooseBannerMsg chooseBannerCmd
            )


view : Model -> Document Msg
view model =
    { title = "Change Plan - Medicare Max"
    , body =
        [ LimitBanner.view model.limitBanner
            |> Html.map ChooseBannerMsg
        , ChoosePlan.viewChangePlan model.choosePlanModel
            |> Html.map ChoosePlanMsg
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ChoosePlanMsg (ChoosePlan.subscriptions model.choosePlanModel)