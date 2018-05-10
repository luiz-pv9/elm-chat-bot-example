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
    , rootBlueprint : Blueprint
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
    | Continue String
    | SendOptions (Dict String Blueprint)
    | Wait Float
    | End


type alias Recipe =
    { action : Action }


type alias Blueprint =
    { id : String
    , recipes : List Recipe
    }


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    let
        recipe =
            List.head session.blueprint.recipes
                |> Maybe.withDefault endRecipe

        recipe2 =
            Debug.log "recipe" recipe

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

        Continue _ ->
            { session | inputState = Blocked }

        End ->
            { session | inputState = Ended }


advanceSession : Msg -> Recipe -> Session -> Session
advanceSession msg recipe session =
    case ( msg, recipe.action ) of
        ( Input choice, SendOptions options ) ->
            { session | blueprint = Dict.get choice options |> Maybe.withDefault session.blueprint }

        ( _, Continue botId ) ->
            { session | blueprint = searchBotById botId session.rootBlueprint |> Maybe.withDefault session.rootBlueprint }

        ( _, _ ) ->
            { session | blueprint = Blueprint session.blueprint.id (List.tail session.blueprint.recipes |> Maybe.withDefault []) }


searchBotById : String -> Blueprint -> Maybe Blueprint
searchBotById botId blueprint =
    if blueprint.id == botId then
        Just blueprint
    else
        searchNestedBotsById botId blueprint


searchNestedBotsById : String -> Blueprint -> Maybe Blueprint
searchNestedBotsById botId blueprint =
    let
        nestedBlueprints =
            List.concatMap
                (\recipe ->
                    case recipe.action of
                        SendOptions options ->
                            Dict.values options

                        _ ->
                            []
                )
                blueprint.recipes
                |> List.filter
                    (\blueprint ->
                        if blueprint.id == botId then
                            True
                        else
                            case searchNestedBotsById botId blueprint of
                                Nothing ->
                                    False

                                Just blueprint ->
                                    True
                    )
    in
        List.head nestedBlueprints


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

        ( _, Continue _ ) ->
            False

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

        Wait _ ->
            Nothing

        Continue _ ->
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
    , rootBlueprint = blueprint
    , inputState = Blocked
    }


blueprint : String -> Blueprint
blueprint botId =
    Blueprint botId []


send : String -> Blueprint -> Blueprint
send msg blueprint =
    { blueprint | recipes = List.append blueprint.recipes [ { action = SendText msg } ] }


options : List ( String, Blueprint ) -> Blueprint -> Blueprint
options options blueprint =
    { blueprint | recipes = List.append blueprint.recipes [ { action = SendOptions (Dict.fromList options) } ] }


wait : Float -> Blueprint -> Blueprint
wait seconds blueprint =
    { blueprint | recipes = List.append blueprint.recipes [ { action = Wait seconds } ] }


continue : String -> Blueprint -> Blueprint
continue botId blueprint =
    { blueprint | recipes = List.append blueprint.recipes [ { action = Continue botId } ] }


end : Blueprint -> Blueprint
end blueprint =
    { blueprint | recipes = List.append blueprint.recipes [ endRecipe ] }


endRecipe : Recipe
endRecipe =
    { action = End }
