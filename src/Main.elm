module Main exposing (main)

import Task
import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


serviceUrl : String
serviceUrl =
    "http://localhost:3000/services"



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view =
            \model ->
                { title = "Docker port registry"
                , body = [ view model ]
                }
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
    , newService : Maybe Service
    , editService : Maybe Service
    }


emptyModel : Model
emptyModel =
    { services = []
    , loadingState = Idle
    , newService = Nothing
    , editService = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, Task.succeed LoadServices |> Task.perform identity )



-- UPDATE


type Msg
    = NoOp
      -- load services
    | LoadServices
    | HandleServicesLoaded (Result Http.Error (List Service))
      --create service
    | ShowAddNewService
    | CloseAddNewService
    | UpdateNewServiceProject Service String
    | UpdateNewServiceDockerPort Service String
    | UpdateNewServiceName Service String
    | UpdateNewServiceComment Service String
    | CreateNewService Service
    | HandleServiceCreated (Result Http.Error Service)
      --edit service
    | ShowEditService Service
    | CloseEditService
    | EditServiceProject Service String
    | EditServiceName Service String
    | EditServiceComment Service String
    | UpdateService Service
    | HandleServiceUpdated (Result Http.Error Service)
      --delete service
    | DeleteService Service
    | HandleServiceDeleted (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( { model | loadingState = Idle }, Cmd.none )

        --load services
        LoadServices ->
            ( { model | loadingState = Loading }, getServices )
        HandleServicesLoaded result ->
            case result of
                Ok services ->
                    ( { model | loadingState = Complete, services = services }, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "error" err
                    in
                    ( { model | loadingState = Error, services = [] }, Cmd.none )

        --create service
        ShowAddNewService ->
            ( { model | newService = Just emptyService }, Cmd.none )

        CloseAddNewService ->
            ( { model | newService = Nothing }, Cmd.none )

        CreateNewService service ->
            ( { model | newService = Nothing }, createService service )

        UpdateNewServiceProject service value ->
            ( { model | newService = Just { service | project = value } }, Cmd.none )

        UpdateNewServiceDockerPort service value ->
            ( { model | newService = Just { service | dockerPort = String.toInt value |> Maybe.withDefault 0 } }, Cmd.none )

        UpdateNewServiceName service value ->
            ( { model | newService = Just { service | name = value } }, Cmd.none )

        UpdateNewServiceComment service value ->
            ( { model | newService = Just { service | comment = value } }, Cmd.none )

        HandleServiceCreated result ->
            case result of
                Ok service ->
                    let
                        services =
                            model.services ++ [ service ]
                    in
                    ( { model | services = services, newService = Nothing }, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "error" err
                    in
                    ( model, Cmd.none )

        --edit service
        ShowEditService service ->
            ( { model | editService = Just service }, Cmd.none )

        CloseEditService ->
            ( { model | editService = Nothing }, Cmd.none )

        EditServiceProject service value ->
            ( { model | editService = Just { service | project = value } }, Cmd.none )

        EditServiceName service value ->
            ( { model | editService = Just { service | name = value } }, Cmd.none )

        EditServiceComment service value ->
            ( { model | editService = Just { service | comment = value } }, Cmd.none )

        UpdateService service ->
            let
                filteredServices =
                    List.filter (isNotService service) model.services

                updatedServices =
                    filteredServices ++ [ service ]
            in
            ( { model | services = updatedServices, editService = Nothing }, updateService service )

        HandleServiceUpdated result ->
            ( model, Cmd.none )

        --delete service
        DeleteService service ->
            let
                services =
                    model.services

                updatedServices =
                    List.filter (isNotService service) services
            in
            ( { model | services = updatedServices }, deleteService service )

        HandleServiceDeleted _ ->
            ( model, Cmd.none )



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
        , header [ class "header" ]
            [ h1 [] [ text "Docker port registry" ]
            , p [] [ text "Keeps track on your docker ports" ]
            , div [] 
            [ case model.newService of
                Nothing ->
                    button [ class "nes-btn is-primary", onClick ShowAddNewService ] [ text "Add New Service" ]

                _ ->
                    Html.text ""
                , button [ class "nes-btn is-secondary", onClick LoadServices ] [ text "Reload list" ]
            ]
            ]
        , case model.newService of
            Just service ->
                viewCreateNewService service

            Nothing ->
                Html.text ""
        , case model.editService of
            Just service ->
                viewEditService service

            Nothing ->
                Html.text ""
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


viewEditService : Service -> Html Msg
viewEditService service =
    section [ class "nes-container with-title" ]
        [ h2 [ class "title" ] [ text "Edit Service" ]
        , div [ class "nes-field" ]
            [ label [] [ text "Project" ]
            , input [ class "nes-input", value service.project, placeholder "Coffee Machine Website", onInput (EditServiceProject service) ] []
            ]
        , div [ class "nes-field" ]
            [ label [] [ text "Port (Cannot be altered)" ]
            , input [ class "nes-input", disabled True, value (String.fromInt service.dockerPort), placeholder "7777" ] []
            ]
        , div [ class "nes-field" ]
            [ label [] [ text "Name" ]
            , input [ class "nes-input", value service.name, placeholder "Web", onInput (EditServiceName service) ] []
            ]
        , div [ class "nes-field" ]
            [ label [] [ text "Comment" ]
            , textarea [ class "nes-textarea", value service.comment, onInput (EditServiceComment service) ] []
            ]
        , div [] 
            [ button [ class "nes-btn is-primary", onClick (UpdateService service) ] [ text "Update" ]
            , button [ class "nes-btn is-secondary", onClick CloseEditService ] [ text "Close" ]
            ]
        ]


viewCreateNewService : Service -> Html Msg
viewCreateNewService service =
    section [ class "nes-container with-title" ]
        [ h2 [ class "title" ] [ text "Add New Service" ]
        , div [ class "nes-field" ]
            [ label [] [ text "Project" ]
            , input [ class "nes-input", value service.project, placeholder "Coffee Machine Website", onInput (UpdateNewServiceProject service) ] []
            ]
        , div [ class "nes-field" ]
            [ label [] [ text "Port" ]
            , input [ class "nes-input", value (String.fromInt service.dockerPort), placeholder "7777", onInput (UpdateNewServiceDockerPort service) ] []
            ]
        , div [ class "nes-field" ]
            [ label [] [ text "Name" ]
            , input [ class "nes-input", value service.name, placeholder "Web", onInput (UpdateNewServiceName service) ] []
            ]
        , div [ class "nes-field" ]
            [ label [] [ text "Comment" ]
            , textarea [ class "nes-textarea", value service.comment, onInput (UpdateNewServiceComment service) ] []
            ]
        , div []
            [ button [ class "nes-btn is-primary", onClick (CreateNewService service) ] [ text "Add" ]
            , button [ class "nes-btn is-secondary", onClick CloseAddNewService ] [ text "Close" ]
            ]
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
        sortedServices =
            List.sortBy .dockerPort services

        children =
            List.map viewService sortedServices
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
            [ button [ class "nes-btn", onClick (ShowEditService service) ] [ text "Edit" ]
            , button [ class "nes-btn is-error", onClick (DeleteService service) ] [ text "Delete" ]
            ]
        ]



-- HTTP


getServices : Cmd Msg
getServices =
    Http.get
        { url = serviceUrl
        , expect = Http.expectJson HandleServicesLoaded listDecoder
        }


createService : Service -> Cmd Msg
createService service =
    Http.post
        { body = encoder service |> Http.jsonBody
        , url = serviceUrl
        , expect = Http.expectJson HandleServiceCreated decoder
        }


deleteService : Service -> Cmd Msg
deleteService service =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = serviceUrl ++ String.fromInt service.dockerPort
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectWhatever HandleServiceDeleted
        }


updateService : Service -> Cmd Msg
updateService service =
    Http.request
        { method = "PUT"
        , headers = []
        , url = serviceUrl ++ String.fromInt service.dockerPort
        , body = encoder service |> Http.jsonBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectJson HandleServiceUpdated decoder
        }


listDecoder : Decode.Decoder (List Service)
listDecoder =
    Decode.list decoder


decoder : Decode.Decoder Service
decoder =
    Decode.map4 Service
        (Decode.field "project" Decode.string)
        (Decode.field "dockerPort" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "comment" Decode.string)


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
