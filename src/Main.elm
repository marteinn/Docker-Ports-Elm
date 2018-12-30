module Main exposing (main)

import Browser
import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, int, list, map4, string)
import Json.Encode as Encode



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Service =
    { project : String
    , dockerPort : Int
    , name : String
    , comment : String
    }


emptyService : Service
emptyService =
    { project = ""
    , dockerPort = 0
    , name = ""
    , comment = ""
    }


type LoadingState
    = Loading
    | Idle
    | Complete
    | Error


type alias Model =
    { services : List Service
    , loadingState : LoadingState
    , showAddNew : Bool
    , newService : Service
    }


emptyModel : Model
emptyModel =
    { services = []
    , loadingState = Idle
    , showAddNew = False
    , newService = emptyService
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, getServices )



-- UPDATE


type Msg
    = NoOp
    | Failure
    | ShowAddNewService
    | CloseAddNewService
    | UpdateNewServiceProject String
    | UpdateNewServiceDockerPort String
    | UpdateNewServiceName String
    | UpdateNewServiceComment String
    | LoadingServices
    | SaveNewService
    | DeleteService Service
    | DeletedService (Result Http.Error ())
    | GotServices (Result Http.Error (List Service))
    | GotService (Result Http.Error Service)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( { model | loadingState = Idle }, Cmd.none )

        Failure ->
            ( { model | loadingState = Error }, Cmd.none )

        LoadingServices ->
            ( { model | loadingState = Loading }, Cmd.none )

        ShowAddNewService ->
            ( { model | showAddNew = True }, Cmd.none )

        CloseAddNewService ->
            ( { model | showAddNew = False }, Cmd.none )

        SaveNewService ->
            ( { model | showAddNew = False }, addNewService model.newService )

        DeleteService service ->
            let
                services = model.services
                updatedServices = List.filter (isNotService service) services
            in ( { model | services = updatedServices }, deleteService service )

        DeletedService _ ->
            (model, Cmd.none)

        UpdateNewServiceProject value ->
            let
                service =
                    model.newService

                updatedService =
                    { service | project = value }
            in
            ( { model | newService = updatedService }, Cmd.none )

        UpdateNewServiceDockerPort value ->
            let
                service =
                    model.newService

                updatedService =
                    { service | dockerPort = String.toInt value |> Maybe.withDefault 0 }
            in
            ( { model | newService = updatedService }, Cmd.none )

        UpdateNewServiceName value ->
            let
                service =
                    model.newService

                updatedService =
                    { service | name = value }
            in
            ( { model | newService = updatedService }, Cmd.none )

        UpdateNewServiceComment value ->
            let
                service =
                    model.newService

                updatedService =
                    { service | comment = value }
            in
            ( { model | newService = updatedService }, Cmd.none )

        GotService result ->
            case result of
                Ok service ->
                    let
                        services =
                            model.services ++ [ service ]
                    in
                    ( { model | services = services, newService = emptyService }, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "error" err
                    in
                    ( model, Cmd.none )

        GotServices result ->
            case result of
                Ok services ->
                    ( { model | loadingState = Complete, services = services }, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "error" err
                    in
                    ( { model | loadingState = Error, services = [] }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UTILS


importCss : String -> Html Msg
importCss href_ =
    node "link" [ rel "stylesheet", href href_ ] []


isNotService : Service -> Service -> Bool
isNotService needle service =
    needle.dockerPort /= service.dockerPort


-- VIEW


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "model" model
    in
    div []
        [ importCss "https://fonts.googleapis.com/css?family=Press+Start+2P"
        , importCss "https://unpkg.com/nes.css/css/nes.min.css"
        , importCss "style.css"
        , header []
            [ h1 [] [ text "Docker ports" ]
            , case model.showAddNew of
                False ->
                    button [ onClick ShowAddNewService ] [ text "Add new service" ]

                True ->
                    Html.text ""
            ]
        , case model.showAddNew of
            False ->
                Html.text ""

            True ->
                viewCreateNewService model.newService
        , case model.loadingState of
            Idle ->
                viewLoader "0" "100"

            Loading ->
                viewLoader "30" "100"

            Error ->
                p [] [ text "Error" ]

            Complete ->
                viewServiceList model.services
        ]


viewCreateNewService : Service -> Html Msg
viewCreateNewService service =
    section [ class "nes-container with-title" ]
        [ h2 [ class "title" ] [ text "Register new port" ]
        , div [ class "nes-field" ]
            [ label [] [ text "Project" ]
            , input [ class "nes-input", value service.project, placeholder "Coffee Machine Website", onInput UpdateNewServiceProject ] []
            ]
        , div [ class "nes-field" ]
            [ label [] [ text "Port" ]
            , input [ class "nes-input", value (String.fromInt service.dockerPort), placeholder "7777", onInput UpdateNewServiceDockerPort ] []
            ]
        , div [ class "nes-field" ]
            [ label [] [ text "Name" ]
            , input [ class "nes-input", value service.name, placeholder "Web", onInput UpdateNewServiceName ] []
            ]
        , div [ class "nes-field" ]
            [ label [] [ text "Comment" ]
            , textarea [ class "nes-textarea", value service.comment, onInput UpdateNewServiceComment ] []
            ]
        , button [ class "nes-btn is-primary", onClick SaveNewService ] [ text "Add" ]
        , button [ class "nes-btn is-secondary", onClick CloseAddNewService ] [ text "Close" ]
        ]


viewLoader : String -> String -> Html Msg
viewLoader min_ max_ =
    section [ class "nes-container with-title" ]
        [ h2 [ class "title" ] [ text "Loading" ]
        , progress
            [ class "nes-progress is-pattern"
            , value min_
            , Html.Attributes.max max_
            ]
            []
        ]


viewServiceList : List Service -> Html Msg
viewServiceList services =
    let
        children =
            List.map viewService services
    in
    section [ class "nes-container with-title" ]
        [ h2 [ class "title" ] [ text "Services" ]
        , div [ class "nes-table-responsive" ]
            [ table [ class "nes-table is-bordered is-centered table-full-width" ]
                [ thead []
                    [ tr []
                        [ td [] [ text "Project" ]
                        , td [] [ text "Port" ]
                        , td [] [ text "Name" ]
                        , td [] [ text "Comment" ]
                        ]
                    ]
                , tbody [] children
                ]
            ]
        ]


viewService : Service -> Html Msg
viewService service =
    tr []
        [ td [] [ text service.project ]
        , td [] [ text (String.fromInt service.dockerPort) ]
        , td [] [ text service.name ]
        , td [] [ text service.comment ]
        , td []
            [ button [ class "nes-btn" ] [ text "Edit" ]
            , button [ class "nes-btn is-error", onClick (DeleteService service)] [ text "Delete" ]
            ]
        ]



-- HTTP


getServices : Cmd Msg
getServices =
    Http.get
        { url = "http://localhost:3000/services"
        , expect = Http.expectJson GotServices listDecoder
        }





addNewService : Service -> Cmd Msg
addNewService service =
    let
        body =
            encoder service |> Http.jsonBody
    in
    Http.post
        { body = body
        , url = "http://localhost:3000/services"
        , expect = Http.expectJson GotService decoder
        }

deleteService : Service -> Cmd Msg
deleteService service =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:3000/services/" ++ (String.fromInt service.dockerPort)
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectWhatever DeletedService
        }


listDecoder : Decoder (List Service)
listDecoder =
    list decoder


decoder : Decoder Service
decoder =
    map4 Service
        (field "project" string)
        (field "dockerPort" int)
        (field "name" string)
        (field "comment" string)


encoder : Service -> Encode.Value
encoder data =
    let
        list =
            [ ( "project", Encode.string data.project )
            , ( "dockerPort", Encode.int data.dockerPort )
            , ( "name", Encode.string data.name )
            , ( "comment", Encode.string data.comment )
            ]
    in
    list
        |> Encode.object
