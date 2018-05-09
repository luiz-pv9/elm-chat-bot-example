module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, keyCode)
import Json.Decode as Json
import Time
import Bot


bot : Bot.Blueprint
bot =
    Bot.blueprint
        |> Bot.sendText "Hello, %name%, I hope you're doing fine."
        |> Bot.wait (1 * Time.second)
        |> Bot.sendText "What programming paradigm do your prefer?"
        |> Bot.sendOptions
            [ ( "Object Oriented", oopChoice )
            , ( "Functional", functionalChoice )
            ]


oopChoice : Bot.Blueprint
oopChoice =
    Bot.blueprint
        |> Bot.sendText "I too like sending messages to objects."
        |> Bot.end


functionalChoice : Bot.Blueprint
functionalChoice =
    Bot.blueprint
        |> Bot.sendText "Immutability is great, isn't it?"
        |> Bot.end


type IdentificationStep
    = AskForName
    | AskForEmail


type Page
    = IdentificationPage IdentificationStep
    | ChatPage Bot.Session


type alias Model =
    { name : String
    , email : String
    , currentPage : Page
    , validationMessage : Maybe String
    , message : String
    }


type Msg
    = NoOp
    | SetName String
    | SetEmail String
    | AttemptToTransitionToEmail
    | AttemptToTransitionToChat
    | SetMessage String
    | SendMessage


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, Cmd.none )


initialSession : Bot.Session
initialSession =
    Bot.sessionWithProfile bot [ ( "name", "Luiz Paulo" ), ( "email", "luiz@onaboutcode.com" ) ]
        |> Bot.update


emptyModel : Model
emptyModel =
    { name = ""
    , email = ""
    , currentPage = ChatPage initialSession
    , validationMessage = Nothing
    , message = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetName name ->
            ( { model | name = name }, Cmd.none )

        SetEmail email ->
            ( { model | email = email }, Cmd.none )

        AttemptToTransitionToEmail ->
            let
                ( validation, nextPage ) =
                    if String.length model.name > 0 then
                        ( Nothing, IdentificationPage AskForEmail )
                    else
                        ( Just "Please fill in your name", IdentificationPage AskForName )
            in
                ( { model | currentPage = nextPage, validationMessage = validation }, Cmd.none )

        AttemptToTransitionToChat ->
            let
                ( validation, nextPage ) =
                    if String.length model.email > 0 then
                        ( Nothing, ChatPage (buildSessionFromModel model) )
                    else
                        ( Just "Please fill in your email", IdentificationPage AskForEmail )
            in
                ( { model | currentPage = nextPage, validationMessage = validation }, Cmd.none )

        SetMessage message ->
            ( { model | message = message }, Cmd.none )

        SendMessage ->
            ( { model | message = "" }, Cmd.none )


buildSessionFromModel : Model -> Bot.Session
buildSessionFromModel model =
    Bot.sessionWithProfile bot [ ( "name", model.name ), ( "email", model.email ) ]


view : Model -> Html Msg
view model =
    case model.currentPage of
        IdentificationPage step ->
            viewIdentification step model

        ChatPage session ->
            viewChat session model


viewIdentification : IdentificationStep -> Model -> Html Msg
viewIdentification step model =
    case step of
        AskForName ->
            viewIdentificationAskForName model.name model.validationMessage

        AskForEmail ->
            viewIdentificationAskForEmail model.email model.validationMessage


viewIdentificationAskForName : String -> Maybe String -> Html Msg
viewIdentificationAskForName name validation =
    div [ class "relative" ]
        [ viewIdentificationHeader
        , div [ class "w-full p-8" ]
            [ viewIdentificationLabel "name" "What's your name?" validation
            , div [ class "relative" ]
                [ input
                    [ id "name"
                    , class "bg-grey-light p-3 rounded block w-full"
                    , placeholder "e.g., Luna Lovegood"
                    , onInput SetName
                    , value name
                    ]
                    []
                , button
                    [ class "absolute pin-t pin-r mt-3 mr-2 text-sm text-blue-dark font-bold"
                    , onClick AttemptToTransitionToEmail
                    ]
                    [ text "Next" ]
                ]
            ]
        , viewIdentificationFooter
        ]


viewIdentificationAskForEmail : String -> Maybe String -> Html Msg
viewIdentificationAskForEmail email validation =
    div [ class "relative" ]
        [ viewIdentificationHeader
        , div [ class "w-full p-8" ]
            [ viewIdentificationLabel "email" "What's your email address?" validation
            , div [ class "relative" ]
                [ input
                    [ id "email"
                    , class "bg-grey-light p-3 rounded block w-full"
                    , placeholder "e.g., luna@hogwarts.com"
                    , onInput SetEmail
                    , value email
                    ]
                    []
                , button
                    [ class "absolute pin-t pin-r mt-3 mr-2 text-sm text-blue-dark font-bold"
                    , onClick AttemptToTransitionToChat
                    ]
                    [ text "Start chat" ]
                ]
            ]
        , viewIdentificationFooter
        ]


viewIdentificationLabel : String -> String -> Maybe String -> Html Msg
viewIdentificationLabel id labelText validation =
    let
        msg =
            case validation of
                Nothing ->
                    text ""

                Just msg ->
                    span [ class "ml-2 text-sm text-red italic" ] [ text msg ]
    in
        div []
            [ label [ for id ] [ text labelText ]
            , msg
            ]


viewIdentificationHeader : Html Msg
viewIdentificationHeader =
    div [ class "bg-indigo p-8 text-white rounded-t-lg" ]
        [ span [ class "font-bold text-xl block" ] [ text "Welcome to Elm Chat Bot" ]
        , span [] [ text "If you're interested, " ]
        , a [ class "text-white", target "_blank", href "https://github.com/luiz-pv9/elm-chat-bot-example" ] [ text "checkout the code" ]
        ]


viewIdentificationFooter : Html Msg
viewIdentificationFooter =
    div [ class "text-xs text-grey text-center" ]
        [ text "Built by "
        , a [ href "https://onaboutcode.com", class "text-grey-dark" ] [ text "Luiz Paulo" ]
        ]


viewChat : Bot.Session -> Model -> Html Msg
viewChat session model =
    div [ class "p-4" ]
        [ div [] (List.map viewChatMessage session.messages)
        , input
            [ onInput SetMessage
            , onEnter SendMessage
            , class "border-2 rounded p-2 block w-full"
            , value model.message
            , placeholder "Hit enter to send"
            ]
            []
        ]


viewChatMessage : Bot.Message -> Html Msg
viewChatMessage message =
    div [] [ text message.body ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not enter"
    in
        on "keydown" (Json.andThen isEnter keyCode)


main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = (\model -> Sub.none)
        }
