module Bot exposing (..)

import Dict exposing (Dict)
import Regex
import Process
import Task


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
    , options : List String
    }


type MessageSender
    = Bot
    | User


type Msg
    = Run
    | Input


type Action
    = SendText String
    | SendOptions (Dict String Blueprint)
    | Wait Float
    | End


type ActionInteractivy
    = WaitsForInput
    | WaitsForCmd
    | Continues
    | Ends


type alias Recipe =
    { action : Action }


type alias Blueprint =
    List Recipe


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    let
        ( nextSession, cmd ) =
            advance session
    in
        case getInteractivity nextSession of
            WaitsForInput ->
                -- If it waits for input we should advance one more step
                -- in order to render the message or procude the cmd.
                oneMoreAdvance nextSession cmd

            WaitsForCmd ->
                -- If it produces a Cmd, we should advance one more step
                -- in order to produce the cmd.
                oneMoreAdvance nextSession cmd

            Continues ->
                -- If it continues (e.g. multiple send messages), then we
                -- recursevily call the update function with the nextSession
                update msg nextSession

            Ends ->
                -- If the bot ended, there is nothing left to do.
                ( nextSession, cmd )


oneMoreAdvance : Session -> Cmd Msg -> ( Session, Cmd Msg )
oneMoreAdvance session cmd =
    let
        ( nextSession, nextCmd ) =
            advance session
    in
        ( nextSession, Cmd.batch [ cmd, nextCmd ] )


advance : Session -> ( Session, Cmd Msg )
advance session =
    let
        recipe =
            List.head session.blueprint |> Maybe.withDefault endRecipe

        nextBlueprint =
            List.tail session.blueprint |> Maybe.withDefault []

        message =
            buildMessageForRecipe session.profile recipe

        cmd =
            buildCmdForRecipe recipe
    in
        case message of
            Nothing ->
                ( { session | blueprint = nextBlueprint }, cmd )

            Just message ->
                ( { session
                    | messages = List.append session.messages [ message ]
                    , blueprint = nextBlueprint
                  }
                , cmd
                )


waits : Session -> Bool
waits session =
    let
        recipe =
            List.head session.blueprint |> Maybe.withDefault endRecipe
    in
        case recipe.action of
            SendOptions _ ->
                True

            Wait _ ->
                True

            _ ->
                False


buildCmdForRecipe : Recipe -> Cmd Msg
buildCmdForRecipe recipe =
    case recipe.action of
        Wait amount ->
            Task.perform (\_ -> Run) (Process.sleep amount)

        _ ->
            Cmd.none


buildMessageForRecipe : Profile -> Recipe -> Maybe Message
buildMessageForRecipe profile recipe =
    case recipe.action of
        SendText text ->
            Just
                { body = interpolateProfile profile text
                , sentBy = Bot
                , options = []
                }

        SendOptions options ->
            Just
                { body = ""
                , sentBy = Bot
                , options = Dict.keys options
                }

        Wait amount ->
            Nothing

        End ->
            Nothing


getInteractivity : Session -> ActionInteractivy
getInteractivity session =
    let
        recipe =
            List.head session.blueprint |> Maybe.withDefault endRecipe
    in
        case recipe.action of
            Wait _ ->
                WaitsForCmd

            SendOptions _ ->
                WaitsForInput

            End ->
                Ends

            _ ->
                Continues


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
