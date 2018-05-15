module Main2 exposing (..)

import Bot2 as Bot
import Http
import Json.Decode as Decode


type BotRequest
    = GithubRequest (Result Http.Error String)
    | RandomCatRequest (Result Http.Error String)


formatter : BotRequest -> String
formatter req =
    case req of
        _ ->
            "Hello world"


root : Bot.Tree BotRequest
root =
    Bot.tree "root" formatter
        |> Bot.ask "Hello there. Tell me a github username." "github_username"
        |> Bot.request "https://api.github.com/users/%github_username%/repos" GithubRequest githubDecoder


githubDecoder =
    Decode.at [ "example" ] Decode.string
