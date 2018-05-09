module Bot exposing (..)

import Dict exposing (Dict)
import Regex


type alias Profile =
    Dict String String


type alias Session =
    { profile : Profile
    , messages : List Message
    , blueprint : Blueprint
    }


type alias Message =
    { body : String
    , sentBy : MessageSender
    }


type MessageSender
    = Bot
    | User


type Action
    = SendText String
    | SendOptions (Dict String Blueprint)
    | Wait Float
    | End


type alias Recipe =
    { action : Action }


type alias Blueprint =
    List Recipe


update : Session -> Session
update session =
    let
        recipe =
            List.head session.blueprint |> Maybe.withDefault endRecipe

        nextBlueprint =
            List.tail session.blueprint |> Maybe.withDefault []

        message =
            buildMessageForRecipe session.profile recipe
    in
        case message of
            Nothing ->
                { session | blueprint = nextBlueprint }

            Just message ->
                { session
                    | messages = List.append session.messages [ message ]
                    , blueprint = nextBlueprint
                }


waitsForInput : Session -> Bool
waitsForInput session =
    let
        recipe =
            List.head session.blueprint |> Maybe.withDefault endRecipe
    in
        case recipe.action of
            SendOptions _ ->
                True

            _ ->
                False


buildMessageForRecipe : Profile -> Recipe -> Maybe Message
buildMessageForRecipe profile recipe =
    case recipe.action of
        SendText text ->
            Just { body = interpolateProfile profile text, sentBy = Bot }

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
    }


blueprint : Blueprint
blueprint =
    []


sendText : String -> Blueprint -> Blueprint
sendText msg blueprint =
    List.append blueprint [ { action = SendText msg } ]


sendOptions : List ( String, Blueprint ) -> Blueprint -> Blueprint
sendOptions options blueprint =
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
