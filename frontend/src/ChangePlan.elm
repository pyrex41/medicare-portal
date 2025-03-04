module ChangePlan exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import ChoosePlan
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode as Decode


type alias Model =
    { choosePlanModel : ChoosePlan.Model
    }


type Msg
    = ChoosePlanMsg ChoosePlan.Msg


init : { key : Nav.Key, session : String, orgSlug : String } -> ( Model, Cmd Msg )
init { key, session, orgSlug } =
    let
        ( choosePlanModel, choosePlanCmd ) =
            ChoosePlan.init orgSlug session key True
    in
    ( { choosePlanModel = choosePlanModel }
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


view : Model -> Document Msg
view model =
    { title = "Change Plan - Medicare Max"
    , body =
        [ ChoosePlan.viewChangePlan model.choosePlanModel
            |> Html.map ChoosePlanMsg
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ChoosePlanMsg (ChoosePlan.subscriptions model.choosePlanModel)
