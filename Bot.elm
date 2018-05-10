module Bot exposing (..)

import Dict exposing (Dict)
import Regex
import Process
import Task


type alias Profile =
    Dict String String


type InputState
    = Blocked
    | TextInput
    | Options (List String)
    | Ended


type alias Session =
    { profile : Profile
    , messages : List Message
    , blueprint : Blueprint
    , inputState : InputState
    }


type alias Message =
    { body : String
    , sentBy : MessageSender
    }


type MessageSender
    = Bot
    | User


type Msg
    = Run
    | DoneWaiting
    | Input String


type Action
    = SendText String
    | SendOptions (Dict String Blueprint)
    | Wait Float
    | End


type alias Recipe =
    { action : Action }


type alias Blueprint =
    List Recipe


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    let
        recipe =
            List.head session.blueprint
                |> Maybe.withDefault endRecipe

        message =
            case msg of
                Run ->
                    buildMessageFromRecipe session.profile recipe

                _ ->
                    Nothing

        nextMessages =
            case message of
                Nothing ->
                    session.messages

                Just message ->
                    List.append session.messages [ message ]

        nextSession =
            { session | messages = nextMessages }
                |> updateInputState recipe
    in
        if shouldWait msg recipe then
            ( nextSession, buildCmdForRecipe recipe )
        else
            update Run (advanceSession msg recipe nextSession)


updateInputState : Recipe -> Session -> Session
updateInputState recipe session =
    case recipe.action of
        SendText _ ->
            { session | inputState = Blocked }

        SendOptions options ->
            { session | inputState = Options (Dict.keys options) }

        Wait _ ->
            { session | inputState = Blocked }

        End ->
            { session | inputState = Ended }


advanceSession : Msg -> Recipe -> Session -> Session
advanceSession msg recipe session =
    case ( msg, recipe.action ) of
        ( Input choice, SendOptions options ) ->
            { session | blueprint = Dict.get choice options |> Maybe.withDefault session.blueprint }

        ( _, _ ) ->
            { session | blueprint = List.tail session.blueprint |> Maybe.withDefault [] }


shouldWait : Msg -> Recipe -> Bool
shouldWait msg recipe =
    case ( msg, recipe.action ) of
        ( _, SendText _ ) ->
            False

        ( Input choice, SendOptions opts ) ->
            False

        ( _, SendOptions _ ) ->
            True

        ( DoneWaiting, Wait _ ) ->
            False

        ( _, Wait _ ) ->
            True

        ( _, End ) ->
            True


buildCmdForRecipe : Recipe -> Cmd Msg
buildCmdForRecipe recipe =
    case recipe.action of
        Wait amount ->
            Task.perform (\_ -> DoneWaiting) (Process.sleep amount)

        _ ->
            Cmd.none


buildMessageFromRecipe : Profile -> Recipe -> Maybe Message
buildMessageFromRecipe profile recipe =
    case recipe.action of
        SendText text ->
            Just
                { body = interpolateProfile profile text
                , sentBy = Bot
                }

        SendOptions options ->
            Nothing

        Wait amount ->
            Nothing

        End ->
            Nothing


interpolateProfile : Profile -> String -> String
interpolateProfile profile msg =
    Dict.toList profile
        |> List.foldr
            (\( key, value ) msg ->
                Regex.replace Regex.All (Regex.regex ("%" ++ key ++ "%")) (\_ -> value) msg
            )
            msg


sessionWithProfile : Blueprint -> List ( String, String ) -> Session
sessionWithProfile blueprint attributes =
    { profile = (Dict.fromList attributes)
    , messages = []
    , blueprint = blueprint
    , inputState = Blocked
    }


blueprint : Blueprint
blueprint =
    []


send : String -> Blueprint -> Blueprint
send msg blueprint =
    List.append blueprint [ { action = SendText msg } ]


options : List ( String, Blueprint ) -> Blueprint -> Blueprint
options options blueprint =
    List.append blueprint [ { action = SendOptions (Dict.fromList options) } ]


wait : Float -> Blueprint -> Blueprint
wait seconds blueprint =
    List.append blueprint [ { action = Wait seconds } ]


end : Blueprint -> Blueprint
end blueprint =
    List.append blueprint [ endRecipe ]


endRecipe : Recipe
endRecipe =
    { action = End }
