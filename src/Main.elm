import Html exposing (..)
import Browser
import Http
import Json.Decode exposing (Decoder, field, map4, list, string)


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
  { name: String
  , dockerPort: String
  , service: String
  , comment: String
  }

type Model
    = Failure
    | Loading
    | SuccessList (List Service)

init : () -> (Model, Cmd Msg)
init _ = 
    (Loading, getServices)


-- UPDATE

type Msg
    = GotServices (Result Http.Error (List Service))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotServices result ->
            case result of
                Ok ports -> 
                    (SuccessList ports, Cmd.none)

                Err err ->
                    let _ = Debug.log "error" err
                    in
                    (Failure, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    let _ = Debug.log "model" model
    in
    div []
        [ 
            h2 [] [ text "Services:" ]
            , viewServiceList model
        ]


viewServiceList : Model -> Html Msg
viewServiceList model =
    case model of
        Failure ->
            text "Error..."

        Loading ->
            text "Loading..."

        SuccessList services ->
            let children = 
                    List.map viewService services
            in
                ul [] children

viewService : Service -> Html Msg
viewService service =
    div [] 
    [ p [] [ text service.name ]
    , p [] [ text service.dockerPort ] 
    , p [] [ text service.service ] 
    , p [] [ text service.comment ] 
    ]


-- HTTP

getServices : Cmd Msg
getServices =
    Http.get
    { url = "http://localhost:8000/src/example.json"
    , expect = Http.expectJson GotServices listDecoder
    }

listDecoder : Decoder (List Service)
listDecoder =
  list decorder

decorder : Decoder Service
decorder =
    map4 Service
        (field "name" string)
        (field "dockerPort" string)
        (field "service" string)
        (field "comment" string)
